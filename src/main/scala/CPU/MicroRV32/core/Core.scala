package CPU.MicroRV32.core

import spinal.core._
import Lib.Sim.spinalConfig._
import CPU.MicroRV32.core.Arithmetic._
import CPU.MicroRV32.Privilige._
//the part of rv32 to generate cpu core

case class CoreConfig(resetVector:Long = 0x80000000l,
                          formalInterface:Boolean = false, //Todo
                          genMul:Boolean = false,
                          genDiv:Boolean = false,
                          genCompressed:Boolean = false,
                          genCsr:Boolean = true,
                          debugPort : Boolean = true,
                          Xlen:Int = 32,
                          withCsrExtension : Boolean = true,
                          withCompressed:Boolean = false){
  def supportDebug = debugPort
  def supportCsr = genCsr
  def supportCompressed = genCompressed
  def supportMul = genMul
  def supportDiv = genDiv
  def hasMulDiv = supportMul | supportDiv
  //no div without the mul
  assert((genMul & !genDiv) | (genMul & genDiv) | (!genDiv & !genMul),
    message = "the Div unit can't support without the Mul")

  val xlen = Xlen
  val compressExtension = withCompressed
  val regfileAddressWidth = 5 //32 regs
}

object InstructionType extends SpinalEnum{
  // RV32I
  val isUndef, isRType, isRImm, isBranch, isLoad, isStore,
  isCT_JAL, isCT_JALR, isLUI, isAUIPC, isECall, isFence, isIllegal,
  // CSR
  isCSR, isCSRImm, isTrapReturn,
  // MUL DIV REM
  isMulDiv = newElement()
}

case class IMem() extends Bundle{
  val instruction = in Bits(32 bits)
  val address = out Bits(32 bits)
  val fetchEnable = out Bool()
  val instructionReady = in Bool()
}

case class DMem() extends Bundle{
  val address = out Bits (32 bits)
  val readData = in Bits (32 bits)
  val writeData = out Bits (32 bits)
  val write = out Bool() // false : read, true : write
  val enable = out Bool()
  val dataType = out Bits (4 bits) //data Type
  val dataReady = in Bool()
}

case class MemoryInterface() extends Bundle{
  val InstructionMem = IMem()
  val DataMem = DMem()
}

case class CoreIO() extends Bundle{
  val memIF = MemoryInterface()
  // cpu halted through ecall
  val halted = out Bool()
  // sync signal, asserted when core is in fetch state
  val fetchSync = out Bool()
  // halting signals for external, memory mapped shutdown
  val halt = in Bool()
  val haltErr = in Bool()
  val debugPort = out Bits(4 bits)
  // interrupt timer
  val irqTimer = in Bool()
}



//connect the signal between the component
class MicroRV32Core(rvConfig: CoreConfig) extends Component {
  val io = CoreIO()

  val PC = Reg(UInt(32 bits)) init (U(rvConfig.resetVector, 32 bits))
  val pcValMux = UInt(32 bits)
  val rdDataMux = Bits(32 bits)
  val csrValMux = Bits(32 bits)
  val dataTypeMux = Bits(4 bits)

  val ctrlLogic = new ControlUnit(rvConfig)
  val irqPending = Bool()

  io.fetchSync := ctrlLogic.io.fetchSync
  io.halted := ctrlLogic.io.halted
  ctrlLogic.io.halt := io.halt | io.haltErr
  ctrlLogic.io.irqPending := irqPending
  io.debugPort := ctrlLogic.io.debugPort

  io.memIF.InstructionMem.address := PC.asBits
  io.memIF.InstructionMem.fetchEnable := ctrlLogic.io.memCtrl.fetchEnable
  ctrlLogic.io.memCtrl.instructionReady := io.memIF.InstructionMem.instructionReady

  when(ctrlLogic.io.pcCtrl.enablePC) {
    PC := pcValMux
  }
  //control the fetch Unit
  val fetchUnit = new FetchUnit(rvConfig)
  fetchUnit.io.data := io.memIF.InstructionMem.instruction
  fetchUnit.io.sample := ctrlLogic.io.fetchCtrl.sample
  fetchUnit.io.pc := PC

  //control the decode Unit
  val decoder = new DecoderUnit(rvConfig)
  decoder.io.instruction := fetchUnit.io.instruction
  ctrlLogic.io.validDecode := decoder.io.validDecode
  ctrlLogic.io.instrType := decoder.io.instType
  ctrlLogic.io.instrFields := decoder.io.fields

  //control the regfile
  val regs = new Regfile( 5, 32) //get the regfile depth
  regs.io.rs1 := decoder.io.fields.rs1.asUInt
  regs.io.rs2 := decoder.io.fields.rs2.asUInt
  regs.io.wr := ctrlLogic.io.regCtrl.regFileWrite
  regs.io.rd := decoder.io.fields.rd.asUInt
  regs.io.rdData := rdDataMux

  //control the ALU
  val alu = new ALU(rvConfig)
  alu.io.operation.funct3 := decoder.io.fields.funct3
  alu.io.operation.funct7 := decoder.io.fields.funct7
  alu.io.operation.shamt := decoder.io.fields.shamt
  alu.io.operation.iType := decoder.io.instType
  alu.io.opA := ctrlLogic.io.aluCtrl.opA.mux(
    opASelect.opRs1 -> regs.io.rs1Data,
    opASelect.opPC -> PC.asBits,
    opASelect.opZero -> B(0, 32 bits)
  )
  alu.io.opB := ctrlLogic.io.aluCtrl.opB.mux(
    opBSelect.opRs2 -> regs.io.rs2Data,
    opBSelect.opImmediate -> decoder.io.immediate,
    opBSelect.opPCInc -> B(fetchUnit.io.pcIncrement, 32 bits),
    opBSelect.opZero -> B(0, 32 bits)
  )
  ctrlLogic.io.aluCtrl.aluBranch := alu.io.outputBool

  //Todo support mul and div?
  //val muldiv = if (rvConfig.hasMulDiv) new MulDivUnit(rvConfig) else null
  val muldivResult = Bits(32 bits)
  val muldivReady = Bool
  val muldivBusy = Bool
  muldivResult := 0
  muldivReady := False
  muldivBusy := False

  val CSRLogic = (rvConfig.withCsrExtension) generate new CSRUnit(rvConfig)
  import RVCSR._
  if (rvConfig.withCsrExtension) {
    CSRLogic.addr := decoder.io.fields.csr.asUInt
    CSRLogic.accessType := decoder.io.csrType
    CSRLogic.newFecth := ctrlLogic.io.csrCtrl.newFetch
    CSRLogic.enable := ctrlLogic.io.csrCtrl.enable
    irqPending := CSRLogic.mip(MIP_MTIP) & CSRLogic.mie(MIP_MTIP) & CSRLogic.mstatus(MSTATUS_MIE)
    csrValMux := ctrlLogic.io.csrCtrl.writeSelect.mux(
      CsrDataSelect.rs1Data -> regs.io.rs1Data,
      CsrDataSelect.csrImm -> B(decoder.io.csr_uimm, 32 bits)
    )
    CSRLogic.wdata := csrValMux
    CSRLogic.timeInterrupt := io.irqTimer
    CSRLogic.rdX0 := decoder.io.fields.rd === 0
    CSRLogic.rs1X0 := decoder.io.fields.rs1 === 0
    CSRLogic.csrimmZero := decoder.io.csr_uimm === 0
    CSRLogic.isCsr := decoder.io.instType === InstructionType.isCSR
    // direct RW for csr registers
    val mcauseMux = Bits(32 bits)
    mcauseMux := ctrlLogic.io.csrCtrl.mcauseSelect.mux(
      MCauseSelect.trapInstrAddrMisalign -> RVCSR.TRAP_EXC_INSTR_ADDR_MISALIGN,
      MCauseSelect.trapIllegalInstr -> RVCSR.TRAP_EXC_ILLEGAL_INSTR,
      MCauseSelect.trapECallMachine -> RVCSR.TRAP_EXC_ECALL_M_MODE,
      MCauseSelect.trapMachineTimerIRQ -> RVCSR.TRAP_MACHINE_TIMER_INTERRUPT
      // default -> B(0, 32 bits)
    )
    when(ctrlLogic.io.trapEntry) {
      // CSRLogic.mcause := RVCSR.TRAP_EXC_ECALL_M_MODE
      CSRLogic.mcause := mcauseMux
      CSRLogic.mtval := PC.asBits
    }
    when(ctrlLogic.io.trapExit) {
      CSRLogic.mstatus(MSTATUS_MIE) := CSRLogic.mstatus(MSTATUS_MPIE)
      CSRLogic.mstatus(MSTATUS_MPIE) := True
    }
    when(ctrlLogic.io.irqEntry) {
      CSRLogic.mstatus(MSTATUS_MPIE) := CSRLogic.mstatus(MSTATUS_MIE) // save old irq enable
      CSRLogic.mstatus(MSTATUS_MIE) := False // disable interrupts while in traphandler per default
      // CSRLogic.mcause := B(32 bits, 31->true, default->false) | 7 // 7 = Machine timer interrupt
      CSRLogic.mcause := mcauseMux
      CSRLogic.mtval := PC.asBits // last valid instruction before irq
      // CSRLogic.mepc := (programCounter + 4).asBits // next pc to execute after irq handler
      CSRLogic.mepc := PC.asBits // next pc to execute after irq handler
      // CSRLogic.mepc := (programCounter + pcIncrement).asBits // next pc to execute after irq handler
    }
  } else {
    irqPending := False
    csrValMux := B(0, 32 bits)
  }


  io.memIF.DataMem.address := alu.io.output
  io.memIF.DataMem.writeData := regs.io.rs2Data
  io.memIF.DataMem.write := ctrlLogic.io.memCtrl.write
  io.memIF.DataMem.enable := ctrlLogic.io.memCtrl.dataEnable
  io.memIF.DataMem.dataType := ctrlLogic.io.memCtrl.dataType.mux(
    MemorySelType.byte -> B"0001",
    MemorySelType.halfWord -> B"0011",
    MemorySelType.word -> B"1111"
  )
  ctrlLogic.io.memCtrl.dataReady := io.memIF.DataMem.dataReady


  val incrPC = UInt(32 bits)
  val jalPC = UInt(32 bits)
  val jalrPC = UInt(32 bits)
  val branchPC = UInt(32 bits)
  val trapPC = UInt(32 bits)
  val mretPC = UInt(32 bits)

  incrPC := PC + fetchUnit.io.pcIncrement

  jalPC := PC + decoder.io.immediate.asUInt
  jalrPC := ((decoder.io.immediate.asUInt + regs.io.rs1Data.asUInt).asBits & ~B(1, 32 bits)).asUInt
  branchPC := PC + decoder.io.immediate.asUInt
  trapPC := U(CSRLogic.mtvec(31 downto 2) << 2, 32 bits)
  mretPC := CSRLogic.mepc.asUInt
  pcValMux := ctrlLogic.io.pcCtrl.pcValSel.mux(
    PCSelect.incrementPC -> incrPC,
    PCSelect.jalPC-> jalPC,
    PCSelect.jalrPC -> jalrPC,
    PCSelect.branchPC -> branchPC,
    PCSelect.trapEntryPC -> trapPC,
    PCSelect.trapExitPC -> mretPC
  )


  val jalMisalign = Bool()
  val jalrMisalign = Bool()
  val branchMisalign = Bool()
  if (!rvConfig.supportCompressed) {
    jalMisalign := (jalPC % 4 === 0) ? False | True
    jalrMisalign := (jalrPC % 4 === 0) ? False | True
    branchMisalign := (branchPC % 4 === 0) ? False | True
  } else {
    jalMisalign := (jalPC % 2 === 0) ? False | True
    jalrMisalign := (jalrPC % 2 === 0) ? False | True
    branchMisalign := (branchPC % 2 === 0) ? False | True
  }

  ctrlLogic.io.exceptions.misalignedJumpTarget := jalMisalign
  ctrlLogic.io.exceptions.misalignedJumpLinkTarget := jalrMisalign
  ctrlLogic.io.exceptions.misalignedBranchTarget := branchMisalign

  // Load data sign extension for signed loads
  val extMemData = Bits(32 bits)
  import const.RV32Fields._
  switch(decoder.io.instType) {
    is(InstructionType.isLoad) {
      switch(decoder.io.fields.funct3) {
        is(F3_LB) {
          extMemData := S(io.memIF.DataMem.readData(7 downto 0), 32 bits).asBits
        }
        is(F3_LH) {
          extMemData := S(io.memIF.DataMem.readData(15 downto 0), 32 bits).asBits
        }
        is(F3_LBU) {
          extMemData := U(io.memIF.DataMem.readData(7 downto 0), 32 bits).asBits
        }
        is(F3_LHU) {
          extMemData := U(io.memIF.DataMem.readData(15 downto 0), 32 bits).asBits
        }
        default {
          extMemData := io.memIF.DataMem.readData
        }
      }
    }
    default {
      extMemData := io.memIF.DataMem.readData
    }
  }
  rdDataMux := ctrlLogic.io.regCtrl.regRdSel.

    mux(
    rdDataSelect.aluResults -> alu.io.output,
    rdDataSelect.aluBool -> B(alu.io.outputBool, 32 bits),
    rdDataSelect.memReadData -> extMemData,
    rdDataSelect.csrReadData -> CSRLogic.rdata,
    rdDataSelect.muldivData -> muldivResult
  )
}

//generator the core here
object Core extends App{
  //set some configs here
  val rtl = RtlConfig()
  rtl.setconfig(new MicroRV32Core(rvConfig = CoreConfig()))
}