package CPU.Nax.fetch

import CPU.Nax.Fetch._
import CPU.Nax.utils._
import CPU.Nax.Interface._
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable
import spinal.lib.bus.amba4.axi._

/*this is about how to implement Fetch Cache
* ICache is ReadOnly
* */
case class FetchL1Cmd(physicalWidth:Int) extends Bundle{
  //fetch ir from L1 Cmd
  val address = UInt(physicalWidth bits)
  val io = Bool()
}

case class FetchL1Rsp(dataWidth:Int) extends Bundle {
  //return fetch error
  val data = Bits(dataWidth bits)
  val error = Bool()
}

case class FetchL1Bus(physicalWidth:Int,
                      dataWidth:Int,
                      lineSize:Int, //Cache line size
                      withBackPresure:Boolean) extends Bundle with IMasterSlave{
  val cmd = Stream(FetchL1Cmd(physicalWidth))
  val rsp = Stream(FetchL1Rsp(dataWidth))
  val beatCount = lineSize * 8 / dataWidth
  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

  //Composite can set the name Area and can get the self Pro //spilt (!_.io)
  //split io cmd and fetch cmd
  def split(selGen:FetchL1Cmd => Bool):(FetchL1Bus,FetchL1Bus) = new Composite(this,"split"){
    //clone into two bus requests
    val bus0,bus1 = cloneOf(self)
    val selCmd = selGen(cmd)
    val selRsp = RegNextWhen(selCmd,cmd.valid)

    bus1.cmd.valid := cmd.valid && selCmd
    bus0.cmd.valid := cmd.valid && !selCmd

    bus0.cmd.payload := cmd.payload
    bus1.cmd.payload := cmd.payload

    //like mux(selCmd,bus1.cmd.ready,bus0.cmd.ready)
    cmd.ready := selCmd ? bus1.cmd.ready | bus0.cmd.ready
    rsp.valid := bus1.rsp.valid || bus0.rsp.valid
    rsp.payload := selRsp ? bus1.rsp.payload | bus0.rsp.payload
    bus1.rsp.ready := rsp.ready
    bus0.rsp.ready := rsp.ready
    val ret = (bus0,bus1)
  }.ret

  def ioSplit():(FetchL1Bus,FetchL1Bus) = split(!_.io) //bus 0 -> get io request

  //Todo resizer

  //convert the cmd and rsp with bus signal
  def toAxi4(): Axi4ReadOnly = new Composite(this, "toAxi4") {
    val axiConfig = Axi4Config(
      addressWidth = physicalWidth,
      dataWidth = dataWidth,
      idWidth = 0,
      useId = true,
      useRegion = false,
      useBurst = true,
      useLock = false,
      useCache = false,
      useSize = true,
      useQos = false,
      useLen = true,
      useLast = true,
      useResp = true,
      useProt = true,
      useStrb = false
    )

    val axi = Axi4ReadOnly(axiConfig)
    axi.ar.valid := cmd.valid
    axi.ar.addr := cmd.address
    axi.ar.id := 0
    axi.ar.prot := B"110"
    axi.ar.len := lineSize * 8 / dataWidth - 1
    axi.ar.size := log2Up(dataWidth / 8)
    axi.ar.setBurstINCR()
    cmd.ready := axi.ar.ready

    rsp.valid := axi.r.valid
    rsp.data := axi.r.data
    rsp.error := !axi.r.isOKAY()
    axi.r.ready := (if (withBackPresure) rsp.ready else True)
  }.axi

}

case class FetchBypassSpec(stageId : Int) extends Area{
  val valid = Stageable(Bool())
  val data = Stageable(WORD)
}

class FetchCachePlugin(var cacheSize : Int,
                       var wayCount : Int,
                       var memDataWidth : Int,
                       var fetchDataWidth : Int,
                       var translationStorageParameter : Any,
                       var translationPortParameter : Any,
                       var lineSize : Int = 64,
                       var readAt : Int = 0,
                       var hitsAt : Int = 1,
                       var hitAt : Int = 1,
                       var bankMuxesAt : Int = 1,
                       var bankMuxAt : Int = 2,
                       var controlAt : Int = 2,
                       var injectionAt : Int = 2,
                       var hitsWithTranslationWays : Boolean = false,
                       var reducedBankWidth : Boolean = false,
                       var tagsReadAsync : Boolean = true,
                       var refillEventId : Int = PerformanceCounterService.ICACHE_REFILL) extends Plugin with FetchPipelineRequirements {
  override def stagesCountMin: Int = injectionAt + 1

}


