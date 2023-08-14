package CPU.TinyCore.Stages

//riscv 32 regfile
import spinal.core._
import spinal.lib._
import CPU.TinyCore.Misc._
import lib.Sim.SpinalSim.PrefixComponent

trait RegfileReadKind
object async extends RegfileReadKind
object sync extends RegfileReadKind

case class RegfileIO() extends Bundle with IMasterSlave {
  //read or write data in the regfile IO
  val rs0 = UInt(5 bits)
  val rs1 = UInt(5 bits)
  val rs0Data = Bits(Global.Xlen bits)
  val rs1Data = Bits(Global.Xlen bits)
  val rd = UInt(5 bits)
  val data = Bits(Global.Xlen bits)
  val write = Bool()

  override def asMaster(): Unit = {
    in(rs0,rs1,rd,data,write)
    out(rs0Data,rs1Data)
  }
}

//test about it
class Regfile(setName:Boolean = true,regfileReadKind: RegfileReadKind = async) extends PrefixComponent{
  val io = master (RegfileIO())

  //set register file name here
  private val registerNames = Seq("zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0_fp", "s1", "a0", "a1", "a2", "a3", "a4",
    "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
  )
  val regfile = Mem(Bits(Global.Xlen bits),32)

  def read(addr:UInt,regfileReadKind: RegfileReadKind): Bits = {
    val data = regfileReadKind match {
      case `async` => regfile.readAsync(addr,writeFirst)
      case `sync` => regfile.readSync(addr)
    }
    val readData = addr.mux(
      U(0) -> B(0,Global.Xlen bits),
      default -> data //use write first
    )
    readData
  }

  //set regs name for debug and more
  if (setName) {
    for (i <- 0 until 32) {
      val regWire = Bits(Global.Xlen bits)
      regWire.setName(s"_${i}_" + registerNames(i))
      regWire := regfile.readAsync(U(i).resized,writeFirst)  //why can't use read
    }
  }

  def write(addr:UInt,data:Bits): Unit = {regfile.write(addr,data)}
  io.rs0Data := read(io.rs0,regfileReadKind)
  io.rs1Data:= read(io.rs1,regfileReadKind)

  when(io.write && io.rd =/= 0){
    write(io.rd,io.data)
  }
}