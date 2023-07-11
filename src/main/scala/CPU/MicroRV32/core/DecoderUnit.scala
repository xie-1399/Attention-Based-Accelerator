package CPU.MicroRV32.core

import CPU.MicroRV32.Privilige._
import spinal.core._
import const._
import spinal.lib._

//the clear formal is from : https://github.com/jameslzhu/riscv-card
// val can define both Reg and wire type

case class DecodedFields() extends Bundle{
  val opcode = out Bits(7 bits) // option type
  val rs1 = out Bits(5 bits)
  val rs2 = out Bits(5 bits)
  val rd = out Bits(5 bits)
  val funct3 = out Bits(3 bits)
  val funct7 = out Bits(7 bits)
  val funct12 = out Bits(12 bits)  //system call
  val shamt = out Bits(5 bits) //shift value
  val csr = out Bits(12 bits) //csr address
}

case class DecodeBundle(rvConfig: CoreConfig) extends Bundle{
  val instruction = in Bits(const.instructionWidth bits)
  //needs out?
  val fields = out (DecodedFields())
  val immediate = out Bits(rvConfig.xlen bits)
  val csr_uimm = out Bits(5 bits)  //from the rs1 field
  val validDecode = out Bool()  // valid decode?

  //ins type includes
  val instType = out (InstructionType())
  val csrType = out(CSRAccessType())
}


class DecoderUnit(rvConfig: CoreConfig) extends Component {
  val io = new DecodeBundle(rvConfig)
  noIoPrefix()
  val extender = new ImmExtensionUnit()

  //some wire signal(32 bits)
  val instruction = Bits(const.instructionWidth bits)
  val opcode = Bits(7 bits)
  val rs1 = Bits(5 bits)
  val rs2 = Bits(5 bits)
  val rd = Bits(5 bits)
  val immediate = Bits(32 bits)
  val funct3 = Bits(3 bits)
  val funct7 = Bits(7 bits)
  val funct12 = Bits(12 bits)
  val shamt = Bits(5 bits)
  val csr = Bits(12 bits)
  val csr_uimm = Bits(5 bits)

  //control type
  val decoded = Bool()
  val iType = InstructionType()
  val csrAccessType = CSRAccessType()

  //set default value
  iType := InstructionType.isUndef
  csrAccessType := CSRAccessType.CSRidle
  decoded := False

  instruction := io.instruction
  extender.io.instruction := io.instruction

  opcode := instruction(6 downto 0)
  rs1 := instruction(19 downto 15)
  rs2 := instruction(24 downto 20)
  rd  := instruction(11 downto 7)
  immediate := B(0).resized //or set B(0,32 bits)
  funct3 := instruction(14 downto 12)
  funct7 := instruction(31 downto 25)
  funct12 := instruction(31 downto 20)
  shamt := instruction(24 downto 20)

  csr := instruction(31 downto 20) // csr address
  csr_uimm := instruction(19 downto 15)

  //switch opcode and control decode
  val rvOpcode = const.Opcode
  val rvFields = const.RV32Fields
  val csrOpcode = CSROpcode

  switch(opcode){
    //illegal opcode
    is(rvOpcode.OP_ILLEGAL){
      decoded := False
      iType := InstructionType.isIllegal
    }
    is(rvOpcode.OP_R){
      when((funct7 === rvFields.F7_Z) | (funct7 === rvFields.F7_O && (funct3 === rvFields.F3_SUB || funct3 === rvFields.F3_SRA))){
        decoded:= True
        iType :=  InstructionType.isRType
      }
      //if with M extension
      if(rvConfig.hasMulDiv){
        when(funct7 === rvFields.F7_MULDIV){
          decoded := True
          iType := InstructionType.isRType
        }
      }

    }

    is(rvOpcode.OP_I_RIMM){
      //func3 == 1`or func3 ==5 equals shift option
      when((funct3 =/= 1 && funct3 =/= 5) || (funct3 === 1 && funct7 === rvFields.F7_Z) || (funct3 === 5 && (funct7 === rvFields.F7_O || funct7 === rvFields.F7_Z))){
        decoded := True
        iType := InstructionType.isRImm
        immediate := extender.io.i_imm
      }
    }

    is(rvOpcode.OP_B_BRANCH){
      when(funct3 === rvFields.F3_BEQ || funct3 === rvFields.F3_BNE || funct3 === rvFields.F3_BLT || funct3 === rvFields.F3_BGE || funct3 === rvFields.F3_BLTU || funct3 === rvFields.F3_BGEU){
        decoded := True
        iType := InstructionType.isBranch
        immediate := extender.io.b_imm
      }
    }

    is(rvOpcode.OP_I_LOAD){
      when(funct3 === rvFields.F3_LB || funct3 === rvFields.F3_LH || funct3 === rvFields.F3_LW || funct3 === rvFields.F3_LBU || funct3 === rvFields.F3_LHU){
        decoded := True
        iType := InstructionType.isLoad
        immediate := extender.io.i_imm
      }
    }

    is(rvOpcode.OP_S_STORE){
      when(funct3 === rvFields.F3_SB || funct3 === rvFields.F3_SH || funct3 === rvFields.F3_SW) {
        decoded := True
        iType := InstructionType.isStore
        immediate := extender.io.s_imm
      }
    }
    is(rvOpcode.OP_U_LUI){
      decoded := True
      iType := InstructionType.isLUI
      immediate := extender.io.u_imm
    }
    is(rvOpcode.OP_U_AUIPC){
      decoded := True
      iType := InstructionType.isAUIPC
      immediate := extender.io.u_imm
    }
    is(rvOpcode.OP_J_JAL){
      decoded := True
      iType := InstructionType.isCT_JAL
      immediate := extender.io.j_imm
    }
    is(rvOpcode.OP_I_JALR){
      decoded := True
      iType := InstructionType.isCT_JALR
      immediate := extender.io.i_imm
    }
    is(rvOpcode.OP_I_FENCE){
      //what is Fence(multi hart or multi threads -> memory model)
      when(funct3 === rvFields.F3_FENCE | funct3 === rvFields.F3_FENCE_I) {
        decoded := True
        iType := InstructionType.isFence
      }
    }

    //ecall opcode = csr opcode
    is(rvOpcode.OP_I_ECALL){
      when(funct12 === rvFields.F12_ECALL && funct3 === 0 && rs1 === 0 && rd ===0){
        decoded := True
        iType := InstructionType.isECall
      }.elsewhen(funct12 === csrOpcode.F12_MRET && rs1 === 0 && funct3 === 0 && rd === 0){
        decoded := True
        iType := InstructionType.isTrapReturn
      }.elsewhen(funct3 =/= csrOpcode.F3_CSR_DECODEMASK){
        //csr read and write instruction
        decoded := True
        iType := InstructionType.isCSR
        when(funct3(2)){
          iType := InstructionType.isCSRImm
        }

        switch(funct3){
          is(csrOpcode.F3_CSRRW,csrOpcode.F3_CSRRWI){
            csrAccessType := CSRAccessType.CSRwrite
          }
          is(csrOpcode.F3_CSRRS, csrOpcode.F3_CSRRSI) {
            csrAccessType := CSRAccessType.CSRset
          }
          is(csrOpcode.F3_CSRRC, csrOpcode.F3_CSRRCI) {
            csrAccessType := CSRAccessType.CSRclear
          }
        }
      }
    }
    //no define instruction
    default{
      decoded := False
      iType := InstructionType.isUndef
    }
  }

  //connect the IO Bundle
  io.fields.opcode := opcode
  io.fields.rs1 := rs1
  io.fields.rs2 := rs2
  io.fields.rd := rd
  io.fields.funct3 := funct3
  io.fields.funct7 := funct7
  io.fields.funct12 := funct12
  io.fields.shamt := shamt
  io.fields.csr := csr

  io.immediate := immediate
  io.instType := iType
  io.csr_uimm := csr_uimm
  io.validDecode := decoded
  io.csrType := csrAccessType
}