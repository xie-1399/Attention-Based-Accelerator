package MicroRV32.core
import spinal.core._
//define some const value of RV
//F12 = funct12  F3 = funct3  F7 = funct7

object const {
  object RVOpcode{
    // opcode format types
    def OP_ZEROS = B"0000000"
    def OP_REGREG = B"0110011"
    def OP_REGIMM = B"0010011"
    def OP_LOAD = B"0000011"
    def OP_STORE = B"0100011"
    def OP_BRANCH = B"1100011"
    def OP_JAL = B"1101111"
    def OP_JALR = B"1100111"
    def OP_LUI = B"0110111"
    def OP_AUIPC = B"0010111"
    def OP_FENCE = B"0001111"
    def OP_ECALL = B"1110011"
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

    def F7_Z = B"0000000"
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
  }

}
