package CPU.TinyCore.Memory

//implement the instruction Cache
import lib.Sim.SpinalSim.PrefixComponent
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

//Todo add test(update it to mul ways)

// I Cache Config and axi4 bus config
case class InstructionCacheConfig(cacheSize: Int,
                                  bytePerLine : Int,
                                  wayCount : Int,
                                  addressWidth : Int,
                                  wrappedMemAccess : Boolean, //if use wrap transport
                                  cpuDataWidth : Int,
                                  memDataWidth : Int){

  def burstSize = bytePerLine * 8/memDataWidth //burst cycle bus can fill one cacheline

  def getAxi4ReadOnlyConfig() = Axi4Config(
    addressWidth = addressWidth,
    dataWidth = 32,
    useId = false,
    useRegion = false,
    useLock = false,
    useQos = false,
    useResp = false,
    useSize = false
  )
}

// the bus transport cpu data to the Cache
case class InstructionCacheCpuCmd()(implicit p:InstructionCacheConfig) extends Bundle{
  val address = UInt(p.addressWidth bits)
}

case class InstructionCacheCpuRsp()(implicit p:InstructionCacheConfig) extends Bundle{
  val address = UInt(p.addressWidth bits)
  val data = Bits(32 bits)
}

case class InstructionCacheCpuBus()(implicit p:InstructionCacheConfig) extends Bundle with IMasterSlave{
  val cmd = Stream(InstructionCacheCpuCmd())
  val rsp = Stream(InstructionCacheCpuRsp())

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

//for fetch instruction from memory
case class InstructionCacheMemCmd()(implicit p : InstructionCacheConfig) extends Bundle{
  val address = UInt(p.addressWidth bit)
}
case class InstructionCacheMemRsp()(implicit p : InstructionCacheConfig) extends Bundle{
  val data = Bits(32 bit)
}

case class InstructionCacheMemBus()(implicit val p : InstructionCacheConfig) extends Bundle with IMasterSlave {
  val cmd = Stream(InstructionCacheMemCmd())
  val rsp = Flow(InstructionCacheMemRsp())

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

  //convert it into Bus request
  def toAxi4ReadOnly():Axi4ReadOnly = {
    val axiConfig = p.getAxi4ReadOnlyConfig()
    val bus = Axi4ReadOnly(axiConfig)
    val burstType = if(p.wrappedMemAccess) B"10" else B"01"

    bus.readCmd.valid := cmd.valid
    bus.readCmd.len := p.burstSize - 1
    bus.readCmd.addr := cmd.address
    bus.readCmd.prot := B"110"
    bus.readCmd.cache := B"1111"
    bus.readCmd.burst := burstType

    cmd.ready := bus.readCmd.ready
    rsp.valid := bus.readRsp.valid
    rsp.data := bus.readRsp.data
    bus.readRsp.ready := True
    bus
  }
}

case class InstructionCacheFlushBus() extends Bundle with IMasterSlave{
  val cmd = Event //noData Stream
  val rsp = Bool()

  override def asMaster(): Unit = {
    master(cmd)
    in(rsp) //just use in
  }
}

class InstructionCache(implicit p:InstructionCacheConfig) extends PrefixComponent{
  assert(p.wayCount == 1) //direct map
  assert(p.memDataWidth == p.cpuDataWidth)
  val io = new Bundle{
    // all slaves show the bus control the Cache
    val flush = slave (InstructionCacheFlushBus()) //whether to flush the instruction Cache
    val cpu = slave (InstructionCacheCpuBus()) //Cpu <-> Cache
    val mem = master (InstructionCacheMemBus()) //Cache <-> Memory
  }
  val haltCpu = False
  val lineWidth = 8 * p.bytePerLine // line bits
  val lineCount = p.cacheSize / p.bytePerLine //all line numbers
  val wordWidth = Math.max(p.memDataWidth,32)
  val wordWidthLog2 = log2Up(wordWidth)
  val wordPerLine = lineWidth / wordWidth
  val bytePerWord = wordWidth / 8
  val wayLineCount = lineCount / p.wayCount
  val wayLineLog2 = log2Up(wayLineCount)
  val wayWordCount = wayLineCount * wordPerLine

  val tagRange = p.addressWidth -1 downto log2Up(wayLineCount * p.bytePerLine) //tag offset
  val lineRange = tagRange.low - 1 downto log2Up(p.bytePerLine) //line offset
  val wordRange = log2Up(p.bytePerLine) -1 downto log2Up(bytePerWord) //word offset

  //Cache Line information
  case class lineInfo() extends Bundle{
    val valid = Bool()
    val address = UInt(tagRange.length bits) //get the tag length
  }

  val ways = Array.fill(p.wayCount)(
    new Area {
      val tags = Mem(lineInfo(),wayLineCount)
      val datas = Mem(Bits(wordWidth bits),wayWordCount)
    }
  )

  //load the cache line(from memory write data and tags to the Cache)
  val lineLoader = new Area {
    val requestIn = Stream(new Bundle{
      val address = UInt(p.addressWidth bits)
    })
    if(p.wrappedMemAccess) io.mem.cmd.address := requestIn.address(tagRange.high downto wordRange.low) @@ U(0,wordRange.low bit)
    //each time get one cache line
    else io.mem.cmd.address := requestIn.address(tagRange.high downto lineRange.low) @@ U(0,lineRange.low bit)

    //flush Counter(when flush the cache,stop the cpu)
    val flushCounter = Reg(UInt(log2Up(wayLineCount) + 1 bits)) init(0)
    when(!flushCounter.msb){
      haltCpu := True
      flushCounter := flushCounter + 1
    }
    when(!RegNext(flushCounter.msb)){
      haltCpu := True
    }

    //flush bus send the flush request
    val flushFromInterface = RegInit(False)
    when(io.flush.cmd.valid){
      haltCpu := True
      when(io.flush.cmd.ready){
        flushCounter := 0
        flushFromInterface := True
      }
    }
    io.flush.rsp := flushFromInterface && flushCounter.msb.rise  //just finish flush

    //write the cacheLine
    val lineInfoWrite = lineInfo()
    lineInfoWrite.valid := flushCounter.msb
    lineInfoWrite.address := requestIn.address(tagRange)
    when(requestIn.fire || !flushCounter.msb){
      //write the tag(if flush just flush the way tags or find the request tag)
      val tagsAddress = Mux(flushCounter.msb,requestIn.address(lineRange),flushCounter(flushCounter.high-1 downto 0))
      ways(0).tags(tagsAddress) := lineInfoWrite
    }

    val request = requestIn.haltWhen(!io.mem.cmd.ready).m2sPipe()
    io.mem.cmd.valid := requestIn.valid && !request.isStall //not ready
    val wordIndex = Reg(UInt(log2Up(wordPerLine) bits))
    val loadedWordsNext = Bits(wordPerLine bits)
    val loadedWords = RegNext(loadedWordsNext)
    val loadedWordsReadable = RegNext(loadedWords)
    loadedWordsNext := loadedWords

    when(io.mem.rsp.fire){
      wordIndex := wordIndex + 1
      loadedWordsNext(wordIndex) := True
      ways(0).datas(request.address(lineRange) @@ wordIndex) := io.mem.rsp.data
    }

    val readyDelay = Reg(UInt(1 bits)) init 0
    when(loadedWordsNext === B(loadedWordsNext.bitsRange -> true)){
      readyDelay := readyDelay + 1
    }
    request.ready := readyDelay === 1
    when(requestIn.ready){
      wordIndex := io.mem.cmd.address(wordRange)
      loadedWords := 0
      loadedWordsReadable := 0
      readyDelay := 0
    }
  }

  //cpu send cmd and get data from Cache
  val task = new Area {
    //send the address and cache hit?
    val request = io.cpu.cmd.haltWhen(haltCpu).m2sPipe()
    request.ready := io.cpu.rsp.fire
    val waysHitValid = False
    val waysHitWord = Bits(wordWidth bits)
    waysHitWord.assignDontCare()

    val waysRead = for(way <- ways) yield new Area {

      val readAddress = Mux(request.isStall,request.address,io.cpu.cmd.address)
      val tag = way.tags.readSync(readAddress(lineRange))
      val data = way.datas.readSync(readAddress(lineRange.high downto wordRange.low))

      //tag hit and valid
      when(tag.valid && tag.address === request.address(tagRange)){
        waysHitValid := True
        waysHitWord := data
      }
    }

    val loaderHitValid = lineLoader.request.valid && lineLoader.request.address(tagRange) === request.address(tagRange)
    val loaderHitReady = lineLoader.loadedWordsReadable(request.address(wordRange))

    io.cpu.rsp.valid := request.valid && waysHitValid && !(loaderHitValid && !loaderHitReady)
    io.cpu.rsp.data := waysHitWord
    io.cpu.rsp.address := request.address
    lineLoader.requestIn.valid := request.valid && !waysHitValid
    lineLoader.requestIn.address := request.address
  }
  io.flush.cmd.ready := !(lineLoader.request.valid || task.request.valid)
}

