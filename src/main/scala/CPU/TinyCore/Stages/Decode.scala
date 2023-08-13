package CPU.TinyCore.Stages

import CPU.TinyCore.RiscvCoreConfig
import spinal.core._
import spinal.lib._
import CPU.TinyCore.DecodeInfo._
import lib.Sim.SpinalSim.PrefixComponent

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
}

case class CoreDecodeOutput()(implicit p:RiscvCoreConfig) extends Bundle {
  val pc = UInt(p.pcWidth bits)
  val instrcution = Bits(32 bits)
  val ctrl = InstructionCtrl()
  val src0 = Bits(32 bits)
  val src1 = Bits(32 bits)
  val alu_op0 = Bits(32 bits)
  val doSub = Bool()
  //Todo add Branch
}

class Decode(implicit p:RiscvCoreConfig) extends PrefixComponent {
  val io = new Bundle{
    //input IR
    val inInst = slave Stream(FetchOutput())  //should pipeline while fetch the data
  }
  val regfile = new Regfile()
  val hazard = Bool()
  val throwIt = False
  val halt = False
  when(hazard){
    halt := True
  }

  //get the src values
  val addr0 = io.inInst.instruction(src0Range)
  val addr1 = io.inInst.instruction(src1Range)
  val addr0IsZero = addr0 === 0
  val addr1IsZero = addr1 === 0

  //Todo read rf add sync
  val srcInstruction = p.regfileReadKind match {
    case `async` => io.inInst.instruction
    case `sync` => io.inInst.instruction
  }
  val regFileReadAddress0 = srcInstruction(src0Range).asUInt
  val regFileReadAddress1 = srcInstruction(src1Range).asUInt

  val (src0,src1) = p.regfileReadKind match {
    case async => (regfile.read(regFileReadAddress0,p.regfileReadKind),regfile.read(regFileReadAddress0,p.regfileReadKind))
    case sync => (regfile.read(regFileReadAddress0,p.regfileReadKind),regfile.read(regFileReadAddress0,p.regfileReadKind))
    case _ =>
  }

  val imm = IMM(io.inInst.instruction)

  //add branch here


  //flush
  val flush = False
  when(flush){
    //Todo let fetch throw it
    throwIt := True
  }
}
