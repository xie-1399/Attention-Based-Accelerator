package CPU.MicroRV32.core

import spinal.core
import spinal.core._
import spinal.lib._
import lib.Logger._
import lib.spinalConfig._
import spinal.lib.misc.{BinTools, HexTools}
//Memory Unit with Simple Bus

case class SimpleBus(dataWidth:Int,addressWidth:Int) extends Bundle with IMasterSlave {
  val address = UInt(dataWidth bits)
  val write = Bool()
  val wdata = Bits(dataWidth bits)
  val size = UInt(4 bits)
  val valid = Bool()
  //slave signal
  val rdata = Bits(dataWidth bits)
  val ready = Bool()

  override def asMaster(): Unit = {
    out(address,write,wdata,size,valid)
    in(ready,rdata)
  }

}

//use the Stream instead can also work
case class Cmd(dataWidth:Int,addressWidth:Int) extends Bundle{
  val address = UInt(dataWidth bits)
  val write = Bool()
  val wdata = Bits(dataWidth bits)
  val size = UInt(4 bits)
}

case class Rsp(dataWidth:Int,addressWidth:Int) extends Bundle{
  val rdata = Bits(dataWidth bits)
}

case class StreamBus(dataWidth:Int,addressWidth:Int) extends Bundle with IMasterSlave {
  //master signal
  val cmd = Stream (Cmd(dataWidth,addressWidth))
  val rsp = Flow (Rsp(dataWidth,addressWidth))
  override def asMaster(): Unit = {
    out(cmd)
    in(rsp)
  }
}

//write memory mask
object MemoryMasks{
  def W_WORDMASK = B"1111"
  def W_HALFMASK_0 = B"0011"
  def W_HALFMASK_1 = B"1100"
  def W_BYTEMASK_0 = B"0001"
  def W_BYTEMASK_1 = B"0010"
  def W_BYTEMASK_2 = B"0100"
  def W_BYTEMASK_3 = B"1000"
}

//a simple memory with the Simple Bus Interface
class Memory(memoryWidth:Bits,wordCount:Int,initFile:String = null) extends Component {
  val io = new Bundle{
    val bus = slave(SimpleBus(32,32))
    val sel = in Bool()
  }

  // get the data width depends on memoryWidth
  val memRam = Mem(memoryWidth,wordCount)
  //no clear and show first time
  logout(s"Memory bitwidth : " + memoryWidth.getWidth.toString + " bits",project = "MicroRV32",showproject = true)
  logout(s"Memory wordcount : " + wordCount)

  if(initFile == null){
    logout("Init RAM with all zero")
    memRam.init(List.fill(wordCount)(B(0,32 bits)))
  }
  else {
    if(initFile.endsWith("elf")){
      logout("Init RAM with initFile " + initFile)
      //Todo about the offset
      HexTools.initRam(memRam,initFile,hexOffset = 0)
    }else if(initFile.endsWith("bin")){
      logout("Init RAM with initFile " + initFile)
      //Todo about the offset
      BinTools.initRam(memRam,initFile)
    }
  }

  val ready = RegInit(False)
  val read = io.sel && io.bus.valid && !io.bus.write
  val write = io.sel && io.bus.valid && io.bus.write

  //let the address in the wordCount
  val addr = io.bus.address(log2Up(wordCount) - 1 downto 0)
  val IntAddress = addr / 4
  //why set the size and alignment delay one cycle
  val size = Reg(UInt(4 bits)) init(0)
  size := io.bus.size
  val alignment = Reg(UInt(2 bits)) init(0)
  alignment := addr(1 downto 0)

  //read memory
  val readVal = memRam.readSync(
    address = IntAddress,
    enable = read
  )
  //how to read one word / half word / byte
  val readByte = readVal.subdivideIn(8 bits)(alignment(1 downto 0))
  val readHalfWord = readVal.subdivideIn(16 bits)(alignment(1 downto 1))
  val readWord = readVal

  io.bus.rdata := size.mux[Bits](
    1 -> readByte.resized,
    2 -> readHalfWord.resized,
    4 -> readWord.resized,
    default -> B"0".resized
  )

  //Write Memory
  import MemoryMasks._
  val bytemask = addr(1 downto 0).mux[Bits](
    0 -> W_BYTEMASK_0,
    1 -> W_BYTEMASK_1,
    2 -> W_BYTEMASK_2,
    3 -> W_BYTEMASK_3
  )
  val halfmask = addr(1 downto 1).mux[Bits](
    0 -> W_HALFMASK_0,
    1 -> W_HALFMASK_1
  )
  val writemask = io.bus.size.mux[Bits](
    1 -> bytemask,
    2 -> halfmask,
    4 -> W_WORDMASK,
    default -> W_WORDMASK
  )

  val WriteData = io.bus.size.mux[Bits](
    1 -> B(io.bus.wdata(7 downto 0), 32 bits),
    2 -> B(io.bus.wdata(15 downto 0), 32 bits),
    4 -> B(io.bus.wdata,32 bits),
    default -> 0
  )

  val wByteShAmt = addr(1 downto 0).mux[UInt](
    0 -> 0,
    1 -> 8,
    2 -> 16,
    3 -> 24
  )

  val wHalfShAmt = addr(1 downto 1).mux[UInt](
    0 -> 0,
    1 -> 16
  )

  val wShift = io.bus.size.mux[UInt](
    1 -> wByteShAmt,
    2 -> wHalfShAmt,
    4 -> 0,
    default -> 0
  )

  val wdata = B(WriteData,32 bits) |<< wShift

  memRam.write(
    address = IntAddress,
    data = wdata,
    enable = write,
    mask = writemask
  )
  //set the ready value
  ready := False
  when((read | write) & io.sel){
    ready := True
  }
  io.bus.ready := ready
}
