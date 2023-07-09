package MicroRV32.core
import spinal.core._
//define some const value of RV
//F12 = funct12  F3 = funct3  F7 = funct7

object const {
  val instructionWidth= 32
  //I Extension
  object Opcode{
    // opcode format types (R I S B U J)
    def OP_ILLEGAL = B"0000000"
    def OP_R = B"0110011" //with Mul and Div and R-Type
    def OP_M = B"0110011" //M Extension equals OP_R
    def OP_I_RIMM = B"0010011"
    def OP_I_LOAD = B"0000011"
    def OP_I_ECALL = B"1110011"
    def OP_CSR = B"1110011"
    def OP_I_JALR = B"1100111"
    def OP_I_FENCE = B"0001111"

    def OP_S_STORE = B"0100011"

    def OP_U_LUI = B"0110111"
    def OP_U_AUIPC = B"0010111"

    def OP_B_BRANCH = B"1100011"

    def OP_J_JAL = B"1101111"

  }



  object RV32Fields{
    def F12_ECALL = M"000000000000" //change privilige
    def F12_EBREAK = M"000000000001" //allow break to use Debugger
    def F3_PRIV = M"000" //Todo

    def F3_JALR = B"000"
    def F3_BEQ = B"000"
    def F3_BNE = B"001"
    def F3_BLT = B"100"
    def F3_BGE = B"101"
    def F3_BLTU = B"110"
    def F3_BGEU = B"111"
    def F3_LB = B"000"
    def F3_LH = B"001"
    def F3_LW = B"010"
    def F3_LBU = B"100"
    def F3_LHU = B"101"
    def F3_SB = B"000"
    def F3_SH = B"001"
    def F3_SW = B"010"
    def F3_ADDI = B"000"
    def F3_SLTI = B"010"
    def F3_SLTIU = B"011"
    def F3_XORI = B"100"
    def F3_ORI = B"110"
    def F3_ANDI = B"111"
    def F3_SLLI = B"001"
    def F3_SRLI = B"101"
    def F3_SRAI = B"101"
    def F3_ADD = B"000"
    def F3_SUB = B"000"
    def F3_SLL = B"001"
    def F3_SLT = B"010"
    def F3_SLTU = B"011"
    def F3_XOR = B"100"
    def F3_SRL = B"101"
    def F3_SRA = B"101"
    def F3_OR = B"110"
    def F3_AND = B"111"
    def F3_FENCE = B"000"
    def F3_FENCE_I = B"001"

    //M F3
    def F3_MUL = M"000"
    def F3_MULH = M"001"
    def F3_MULHSU = M"010"
    def F3_MULHU = M"011"
    // DIV
    def F3_DIV = M"100"
    def F3_DIVU = M"101"
    // REM
    def F3_REM = M"110"
    def F3_REMU = M"111"
    def F3_MUL_OPERATION = M"0--"
    def F3_DIV_OPERATION = M"1--"
    def F3_DIV_DIVU = M"10-"
    def F3_REM_REMU = M"11-"


    def F7_Z = B"0000000" // R type
    def F7_O = B"0100000"
    def F7_SLLI = B"0000000"
    def F7_SRLI = B"0000000"
    def F7_SRAI = B"0100000"
    def F7_ADD = B"0000000"
    def F7_SUB = B"0100000"
    def F7_SLL = B"0000000"
    def F7_SLT = B"0000000"
    def F7_SLTU = B"0000000"
    def F7_XOR = B"0000000"
    def F7_SRL = B"0000000"
    def F7_SRA = B"0000000"
    def F7_OR = B"0000000"
    def F7_AND = B"0000000"
    def F7_MULDIV = M"0000001"  // M Type

  }

}
