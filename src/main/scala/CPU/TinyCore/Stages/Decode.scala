package CPU.TinyCore.Stages

import CPU.TinyCore.RiscvCoreConfig
import spinal.core._
import spinal.lib._

//Decode for the Fetch Instruction

case class IMM(instruction:Bits) extends Area{
  //get imm value
  def i = instruction(32 downto 20)
  def s = instruction(31 downto 25) ## instruction(11 downto 7)
  def b = instruction(31) ## instruction(7) ## instruction(30 downto 25) ## instruction(11 downto 8)
  def u = instruction(31 downto 12) ## U"x000"
  def j = instruction(31) ## instruction(19 downto 12) ## instruction(20) ## instruction(30 downto 21)
  def z = instruction(19 downto 15)
  //get signal imm value
  def i_sext = B((19 downto 0) -> i(11)) ## i
  def s_sext = B((19 downto 0) -> s(11)) ## s
  def b_sext = B((18 downto 0) -> b(11)) ## b ## False
  def j_sext = B((10 downto 0) -> j(19)) ## j ## False

  def src0Range = 19 downto 15
  val src1Range = 24 downto 20
  val rdRange = 11 downto 7
}


case class CoreDecodeOutput()(implicit p:RiscvCoreConfig) extends Bundle {
  val pc = UInt(p.pcWidth bits)
  val instrcution = Bits(32 bits)
  //val ctrl =
}

class Decode(implicit p:RiscvCoreConfig) extends Component {
  val io = new Bundle{
    //input IR
    val inInst = in Bits(32 bits)
  }
  val hazard = Bool()
  val throwIt = False
  val halt = False
  when(hazard){
    halt := True
  }

  //get the src values


}
