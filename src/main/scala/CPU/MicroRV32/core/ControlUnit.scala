package CPU.MicroRV32.core

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

/*
control the core work
using one stateMachine to control the pc and deal the trap
decide write back to the register or Memory
*/
object PCSelect extends SpinalEnum{
  val incrementPC,jalPC,jalrPC,branchPC,
  trapEntryPC,trapExitPC = newElement()
}

case class PCCtrl() extends Bundle{
  //Control the PC value
  val enablePC = out Bool()
  val pcValSel = out (PCSelect())
}

case class FetchCtrl() extends Bundle{
  val sample = out Bool()
}

object opASelect extends SpinalEnum{
  //opA's kinds of value
  val opRs1,opPC,opZero = newElement()
}

object opBSelect extends SpinalEnum{
  //opB's value
  val opRs2,opImmediate,opPCInc,opZero = newElement()
}

case class ALUCtrl() extends Bundle{
  //whether is Branch instruction
  val opA = out(opASelect())
  val opB = out(opBSelect())
  val aluBranch = in Bool()
}


case class MulDivCtrl() extends Bundle{
  //Todo set Mul and Div Ctrl here
  val valid = out Bool()
  val ready = in Bool()
  val busy = in Bool()
}

object rdDataSelect extends SpinalEnum{
  val aluResults,aluBool,memReadData,csrReadData,muldivData = newElement()
}

case class RegFileCtrl() extends Bundle{
  val regFileWrite = out Bool()
  val regRdSel = out (rdDataSelect())
}

object CsrDataSelect extends SpinalEnum{
  val rs1Data,csrImm = newElement()
}

object MCauseSelect extends SpinalEnum{
  //define the trap cause
  val trapInstrAddrMisalign, trapIllegalInstr, trapECallMachine, trapMachineTimerIRQ = newElement()
}

case class CSRCtrl() extends Bundle{
  val writeSelect = out(CsrDataSelect())
  val enable = out Bool()
  val newFetch = out Bool()
  val illegalAccess = in Bool()
  val mcauseSelect = out (MCauseSelect())
}

//select which data type to store
object MemorySelType extends SpinalEnum{
  val byte,halfWord,word = newElement()
}

case class MemoryCtrl() extends Bundle{
  //I-Memory
  val fetchEnable = out Bool()
  val instructionReady = in Bool()

  //D-Memory
  val dataType = out(MemorySelType())
  val write = out Bool()
  val dataEnable = out Bool()
  val dataReady = in Bool()
}

case class ExceptionCtrl() extends Bundle{
  val misalignedJumpTarget = in Bool()
  val misalignedJumpLinkTarget = in Bool()
  val misalignedBranchTarget = in Bool()
}

//set all bundles of control
case class ControlBundle(rvConfig: CoreConfig) extends Bundle {
  //set the control mode control all
  val validDecode = in Bool()
  val instrType = in(InstructionType())
  val instrFields = in(DecodedFields())

  val pcCtrl = out(PCCtrl())
  val fetchCtrl = out(FetchCtrl())
  val aluCtrl = ALUCtrl()
  val muldivCtrl = if (rvConfig.hasMulDiv) MulDivCtrl() else null
  val regCtrl = out(RegFileCtrl())
  val csrCtrl = CSRCtrl()
  val memCtrl = MemoryCtrl()

  val irqPending = in Bool()
  val trapEntry = out Bool()
  val trapExit = out Bool()
  val irqEntry = out Bool()
  val exceptions = in (ExceptionCtrl())
  val halt = in Bool()
  val halted = out Bool()
  val fetchSync = out Bool()
  // debug output
  val debugPort = out Bits (4 bits)
}

class ControlUnit(rvConfig: CoreConfig) extends Component{
  val io = ControlBundle(rvConfig)
  noIoPrefix()
  //default value of the control signal
  io.pcCtrl.enablePC := False
  io.pcCtrl.pcValSel := PCSelect.incrementPC

  io.fetchCtrl.sample := False

  io.aluCtrl.opA := opASelect.opRs1
  io.aluCtrl.opB := opBSelect.opRs2

  io.regCtrl.regFileWrite := False
  io.regCtrl.regRdSel := rdDataSelect.aluResults

  io.csrCtrl.newFetch := False
  io.csrCtrl.enable := False
  io.csrCtrl.mcauseSelect := MCauseSelect.trapECallMachine
  io.csrCtrl.writeSelect := CsrDataSelect.rs1Data

  io.memCtrl.fetchEnable := False
  io.memCtrl.write := False
  io.memCtrl.dataEnable := False
  io.memCtrl.dataType := MemorySelType.word

  io.trapEntry := False
  io.trapExit := False
  io.irqEntry := False

  //IO request
  io.halted := False
  io.fetchSync := False
  if(rvConfig.hasMulDiv){
    io.muldivCtrl.valid := False
  }

  //one state machine control all process
  val fsm = new StateMachine{
    import InstructionType._
    import const.RV32Fields._
    import CPU.MicroRV32.Privilige.CSROpcode._
    //the control logic picture is from
    val stateInit = new State with EntryPoint
    val stateFetch,stateDecode,stateExcute,stateWriteBack,stateTrap,stateCSR,stateInterrupt,stateHalt = new State

    stateInit.whenIsActive{
      goto(stateFetch)
    }

    stateFetch.whenIsActive{
      //has time interrupt
      when(io.irqPending){
        goto(stateInterrupt)
      }.elsewhen(io.memCtrl.instructionReady){
        //fetch instruction
        io.csrCtrl.newFetch := True
        io.fetchCtrl.sample := True
        goto(stateDecode)
      }.otherwise{
        //just wait for memory ready
        io.fetchSync := True
        io.memCtrl.fetchEnable := True
      }
    }
    stateFetch.onExit{
      io.memCtrl.fetchEnable := False
    }

    stateDecode.whenIsActive{
      //valid decode
      when(io.validDecode){
        goto(stateExcute)
      }.otherwise{
        goto(stateTrap)
      }
    }

    stateExcute.whenIsActive{
      io.pcCtrl.enablePC := True
      io.pcCtrl.pcValSel := PCSelect.incrementPC
      switch(io.instrType){
        is(isRType){
          io.aluCtrl.opA := opASelect.opRs1
          io.aluCtrl.opB := opBSelect.opRs2
          io.regCtrl.regFileWrite := True
          io.regCtrl.regRdSel := rdDataSelect.aluResults
          if(rvConfig.hasMulDiv){
            //check the mul and div arithmetic first
            when(io.instrFields.funct7 === F7_MULDIV) {
              io.regCtrl.regFileWrite := False
              io.regCtrl.regRdSel := rdDataSelect.muldivData
              io.aluCtrl.opA := opASelect.opRs1
              io.aluCtrl.opB := opBSelect.opRs2
              io.muldivCtrl.valid := True
              goto(stateWriteBack)
            }.elsewhen(io.instrFields.funct7 === F7_Z | io.instrFields.funct7 === F7_O) {
              goto(stateFetch)
            }.otherwise {
              io.regCtrl.regFileWrite := False
              goto(stateTrap)
            }
          }else{
            when(io.instrFields.funct7 === F7_Z | io.instrFields.funct7 === F7_O){
              goto(stateFetch)
            }.otherwise{
              io.regCtrl.regFileWrite := False
              goto(stateTrap)
            }
          }
        }

        is(isRImm){
          io.aluCtrl.opA := opASelect.opRs1
          io.aluCtrl.opB := opBSelect.opImmediate
          io.regCtrl.regFileWrite := True
          io.regCtrl.regRdSel := rdDataSelect.aluResults
          goto(stateFetch)
        }
        //rd = pc + imm
        is(isAUIPC){
          io.aluCtrl.opA := opASelect.opPC
          io.aluCtrl.opB := opBSelect.opImmediate
          io.regCtrl.regFileWrite := True
          io.regCtrl.regRdSel := rdDataSelect.aluResults
          goto(stateFetch)
        }
        // rd = imm + 0
        is(isLUI){
          io.aluCtrl.opA := opASelect.opZero
          io.aluCtrl.opB := opBSelect.opImmediate
          io.regCtrl.regFileWrite := True
          io.regCtrl.regRdSel := rdDataSelect.aluResults
          goto(stateFetch)
        }

        is(isCT_JAL){
          //save pc before jump
          io.aluCtrl.opA := opASelect.opPC
          io.aluCtrl.opB := opBSelect.opPCInc
          io.regCtrl.regRdSel := rdDataSelect.aluResults
          //pc = pc + imm
          io.pcCtrl.pcValSel := PCSelect.jalPC
          when(io.exceptions.misalignedJumpTarget){
            goto(stateTrap)
          }.otherwise{
            io.pcCtrl.enablePC := True
            io.regCtrl.regFileWrite := True
            goto(stateFetch)
          }
        }

        //Todo use rs1 data as base address
        is(isCT_JALR){
          io.aluCtrl.opA := opASelect.opPC
          io.aluCtrl.opB := opBSelect.opPCInc
          io.regCtrl.regRdSel := rdDataSelect.aluResults
          //pc = pc + imm
          io.pcCtrl.pcValSel := PCSelect.jalrPC
          when(io.exceptions.misalignedJumpLinkTarget) {
            goto(stateTrap)
          }.otherwise {
            io.pcCtrl.enablePC := True
            io.regCtrl.regFileWrite := True
            goto(stateFetch)
        }

        is(isBranch){
          io.aluCtrl.opA := opASelect.opRs1
          io.aluCtrl.opB := opBSelect.opRs2
          //take the Branch
          when(io.aluCtrl.aluBranch){
            io.pcCtrl.pcValSel := PCSelect.branchPC
            when(io.exceptions.misalignedBranchTarget){
              goto(stateTrap)
            }.otherwise{
              io.pcCtrl.enablePC := True
              goto(stateFetch)
            }
          }.otherwise{
            //no branch
            goto(stateFetch)
          }
        }

        is(isLoad){
          io.aluCtrl.opA := opASelect.opRs1
          io.aluCtrl.opB := opBSelect.opImmediate
          io.memCtrl.dataEnable := True
          io.memCtrl.write := False
          switch(io.instrFields.funct3) {
            is(F3_LB, F3_LBU) {
              io.memCtrl.dataType := MemorySelType.byte
            }
            is(F3_LH, F3_LHU) {
              io.memCtrl.dataType := MemorySelType.halfWord
            }
            is(F3_LW) {
              io.memCtrl.dataType := MemorySelType.word
            }
            default {
              io.memCtrl.dataType := MemorySelType.word
            }
          }
          goto(stateWriteBack)
        }

        is(isStore){
          io.aluCtrl.opA := opASelect.opRs1
          io.aluCtrl.opB := opBSelect.opImmediate
          io.memCtrl.dataEnable := True
          io.memCtrl.write := True
          switch(io.instrFields.funct3) {
            is(F3_SB) {
              io.memCtrl.dataType := MemorySelType.byte
            }
            is(F3_SH) {
              io.memCtrl.dataType := MemorySelType.halfWord
            }
            is(F3_SW) {
              io.memCtrl.dataType := MemorySelType.word
            }
            default {
              io.memCtrl.dataType := MemorySelType.word
            }
          }
          goto(stateWriteBack)
        }

        is(isECall,isCSR,isCSRImm,isTrapReturn){
          //if ecall with trap
          when(io.instrFields.funct12 === F12_ECALL && io.instrFields.rs1 === 0 && io.instrFields.funct3 === 0 && io.instrFields.rd === 0){
            io.trapEntry := True
            io.pcCtrl.pcValSel := PCSelect.trapEntryPC
            io.pcCtrl.enablePC := True
            goto(stateFetch)
          }.elsewhen(io.instrFields.funct12 === F12_MRET && io.instrFields.rs1 === 0 && io.instrFields.funct3 === 0 && io.instrFields.rd === 0){
            io.trapExit := True
            io.pcCtrl.pcValSel := PCSelect.trapExitPC
            io.pcCtrl.enablePC := True
            goto(stateFetch)
            //csr read/write instruction
          }.elsewhen(io.instrFields.funct3 =/= F3_CSR_DECODEMASK){
            io.csrCtrl.enable := True
            io.regCtrl.regRdSel := rdDataSelect.csrReadData
            switch(io.instrType){
              is(InstructionType.isCSR){
                io.csrCtrl.writeSelect := CsrDataSelect.rs1Data
              }
              is(InstructionType.isCSRImm){
                io.csrCtrl.writeSelect := CsrDataSelect.csrImm
              }
            }
            goto(stateCSR)
          }
        }

        is(isFence){
          //nothing todo with the fence
          goto(stateFetch)
        }
        is(isIllegal){
          goto(stateTrap)
        }
        default{
           goto(stateHalt)
        }
      }
    }
  }
    stateWriteBack.whenIsActive{
        switch(io.instrType){
          io.aluCtrl.opA := opASelect.opRs1
          io.aluCtrl.opB := opBSelect.opImmediate
          is(isLoad){
            io.memCtrl.dataEnable := True
            switch(io.instrFields.funct3) {
              is(F3_LB, F3_LBU) {
                io.memCtrl.dataType := MemorySelType.byte
              }
              is(F3_LH, F3_LHU) {
                io.memCtrl.dataType := MemorySelType.halfWord
              }
              is(F3_LW) {
                io.memCtrl.dataType := MemorySelType.word
              }
              default {
                io.memCtrl.dataType := MemorySelType.word
              }
            }
          }
          is(isStore){
            io.memCtrl.dataEnable := True
            io.memCtrl.write := True
            switch(io.instrFields.funct3) {
              is(F3_SB) {
                io.memCtrl.dataType := MemorySelType.byte
              }
              is(F3_SH) {
                io.memCtrl.dataType := MemorySelType.halfWord
              }
              is(F3_SW) {
                io.memCtrl.dataType := MemorySelType.word
              }
              default {
                io.memCtrl.dataType := MemorySelType.word
              }
            }
          }
        }
      when(io.memCtrl.dataReady && !io.halt){
        switch(io.instrType){
          is(isLoad){
            io.regCtrl.regFileWrite := True
            io.regCtrl.regRdSel := rdDataSelect.memReadData
          }
        }
        goto(stateFetch)
      }
      if(rvConfig.hasMulDiv) {
        when(io.muldivCtrl.ready & !io.halt) {
          io.regCtrl.regFileWrite := True
          io.regCtrl.regRdSel := rdDataSelect.muldivData
          goto(stateFetch)
        }
      }
      when(io.halt){
        goto(stateHalt)
      }
    }

    stateTrap.whenIsActive{
      io.trapEntry := True
      switch(io.instrType){
        is(isCT_JAL,isCT_JALR,isBranch){
          io.csrCtrl.mcauseSelect := MCauseSelect.trapInstrAddrMisalign
        }

        is(isRType){
          when(!(io.instrFields.funct7 === F7_Z | io.instrFields.funct7 === F7_O)) {
            io.csrCtrl.mcauseSelect := MCauseSelect.trapIllegalInstr
          }
        }
        is(isIllegal,isUndef){
          io.csrCtrl.mcauseSelect := MCauseSelect.trapIllegalInstr
        }
        is(isECall){
          io.csrCtrl.mcauseSelect := MCauseSelect.trapECallMachine
        }
        default{
          //unknown error
          io.csrCtrl.mcauseSelect := MCauseSelect.trapIllegalInstr
        }
      }
      io.pcCtrl.pcValSel := PCSelect.trapEntryPC
      io.pcCtrl.enablePC := True
      goto(stateFetch)
    }

    stateCSR.whenIsActive{
      io.regCtrl.regFileWrite := True
      io.regCtrl.regRdSel := rdDataSelect.csrReadData
      goto(stateFetch)
    }

    stateInterrupt.whenIsActive{
      //just receive the Timer interrupt
      io.csrCtrl.mcauseSelect := MCauseSelect.trapMachineTimerIRQ
      io.irqEntry := True
      io.pcCtrl.enablePC := True
      io.pcCtrl.pcValSel := PCSelect.trapEntryPC
      goto(stateFetch)
    }

    stateHalt.whenIsActive{
      io.halted := True
    }
  }

  //show the state of the StateMachine
  val debugPort = (rvConfig.debugPort) generate new Area {
    io.debugPort := B"0000"
    when(fsm.isActive(fsm.stateInit)) {
      io.debugPort := B"0000"
    }.elsewhen(fsm.isActive(fsm.stateFetch)) {
      io.debugPort := B"0001"
    }.elsewhen(fsm.isActive(fsm.stateDecode)) {
      io.debugPort := B"0010"
    }.elsewhen(fsm.isActive(fsm.stateExcute)) {
      io.debugPort := B"0011"
    }.elsewhen(fsm.isActive(fsm.stateWriteBack)) {
      io.debugPort := B"0100"
    }.elsewhen(fsm.isActive(fsm.stateCSR)) {
      io.debugPort := B"0101"
    }.elsewhen(fsm.isActive(fsm.stateTrap)) {
      io.debugPort := B"0110"
    }.elsewhen(fsm.isActive(fsm.stateHalt)) {
      io.debugPort := B"0111"
    }.elsewhen(fsm.isActive(fsm.stateInterrupt)) {
      io.debugPort := B"1001"
    }
  }

}