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
  val alu_op1 = Bits(32 bits)
  val doSub = Bool()
  //for branch prediction and
}

class Decode(implicit p:RiscvCoreConfig) extends PrefixComponent {
  val io = new Bundle{
    //input IR
    val inInst = slave Stream FetchOutput()  //should pipeline while fetch the data
    val regfileIO = master Stream RegfileIO() //send the RegfileIo to the regfile
    val decodeOutput = master Stream CoreDecodeOutput()
  }

  val hazard = Bool()
  val throwIt = False
  val halt = False
  when(hazard){
    halt := True
  }
  val ctrl = InstructionCtrl(io.inInst.instruction)   //get the ctrl

  //get the src values
  val addr0 = io.inInst.instruction(src0Range)
  val addr1 = io.inInst.instruction(src1Range)
  val addr0IsZero = addr0 === 0
  val addr1IsZero = addr1 === 0

  val srcInstruction = p.regfileReadKind match {  //Todo just use asyc
    case `async` => io.inInst.instruction
    case _ => io.inInst.instruction
  }
  val regFileReadAddress0 = srcInstruction(src0Range).asUInt
  val regFileReadAddress1 = srcInstruction(src1Range).asUInt

  val imm = IMM(io.inInst.instruction)

  //calculate branch/jump target
  val brjumpImm = Mux(io.decodeOutput.payload.ctrl.jump,imm.j_sext,imm.b_sext)
  val brjumpPc = (io.inInst.payload.pc + brjumpImm.asUInt).resize(p.pcWidth)

  //Todo add branch prediction
  val shouldTakenBranch = Bool()
  p.branchPrediction match {
    case `disable` => shouldTakenBranch := False
  }

  //branch jump
  val pcLoad = Flow(UInt(p.pcWidth bits))
  pcLoad.valid := io.inInst.valid && !throwIt && !hazard && io.decodeOutput.ready && (ctrl.br =/= BR.JR && ctrl.br =/= BR.N) && ctrl.illegal && shouldTakenBranch
  pcLoad.payload := brjumpPc

  io.decodeOutput.arbitrationFrom(io.inInst.throwWhen(throwIt).haltWhen(halt))
  io.decodeOutput.pc := io.inInst.pc
  io.decodeOutput.instrcution := io.inInst.instruction
  io.decodeOutput.ctrl := ctrl
  io.decodeOutput.doSub := io.decodeOutput.ctrl.alu =/= ALU.ADD
  io.decodeOutput.src0 := io.regfileIO.payload.rs0Data
  io.decodeOutput.src1 := io.regfileIO.payload.rs1Data
  io.decodeOutput.alu_op0 := io.decodeOutput.ctrl.op0.mux(
    default -> io.decodeOutput.src0,
    OP0.IMU -> imm.u.resized,
    OP0.IMZ -> imm.z.resized,
    OP0.IMJB -> brjumpImm
  )
  io.decodeOutput.alu_op1 := io.decodeOutput.ctrl.op1.mux(
    default -> io.decodeOutput.src1,
    OP1.IMI -> imm.i_sext.resized,
    OP1.IMS -> imm.s_sext.resized,
    OP1.PC -> io.inInst.pc.asBits
  )

  //flush
  val flush = False
  when(flush){
    throwIt := True
  }
}
