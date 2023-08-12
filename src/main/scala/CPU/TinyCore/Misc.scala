package CPU.TinyCore
import spinal.core._

object Misc {
  object Global{
    def Xlen = 32
  }
  object Config{
    val config = RiscvCoreConfig()  //just set some config here
  }

}

//Decode information here
object DecodeInfo{
  //pc type
  object PC extends SpinalEnum(binarySequential) {
    //JR = jalr(pc = rs1 + imm)
    val INC, BRA, J, JR = newElement()
  }

  //Branch type
  object BR extends SpinalEnum {
    val N, NE, EQ, GE, GEU, LT, LTU, J, JR = newElement()
    defaultEncoding = SpinalEnumEncoding("branch")(
      EQ -> 0,
      NE -> 1,
      J -> 2,
      JR -> 3,
      LT -> 4, //less(<)
      GE -> 5, //grater(>=)
      LTU -> 6,
      GEU -> 7,
      N -> 8 // Not a branch
    )

    def isSignComp(that: C) = !that.asBits(1) //sign or unsign branch
  }

  //option num type
  object OP0 extends SpinalEnum(binarySequential) {
    // IMU ->(like lui) / IMJB(JB IMM) /  IMZ -> for the csr op
    val RS, IMU, IMZ, IMJB = newElement()
    def initial = RS
  }

  object OP1 extends SpinalEnum(binarySequential) {
    val RS, IMI, IMS, PC = newElement()
    def initial = RS
  }

  object MemoryOp extends SpinalEnum(binarySequential){
    val LOAD,STORE = newElement()
    def initial = LOAD
  }

  object CSR extends SpinalEnum(binarySequential){
    //Todo
  }

  //set the Alu type
  object ALU extends SpinalEnum {
    val ADD, SLL, SLT, SLTU, XOR, SRL, OR, AND, SUB, SRA, COPY = newElement()
    defaultEncoding = SpinalEnumEncoding("alu")(
      ADD -> 0,
      SLL -> 1,
      SLT -> 2,
      SLTU -> 3,
      XOR -> 4,
      SRL -> 5,
      OR -> 6,
      AND -> 7,
      SUB -> (8 + 0),
      SRA -> (8 + 5),
      COPY -> 15
    )

    def initial = ADD

    def isSltX(that: C) = that.asBits === M"-01-" //slt or sltu

    def isAddSub(that: C) = that.asBits === M"-000" // add or sub
  }

  case class InstructionCtrl() extends Bundle{
    //Todo add more information here like bypass
    val illegal = Bool()  //illegal instruction
    val br = BR()
    val jump = Bool()
    val op0 = OP0()
    val op1 = OP1()
    val alu = ALU()
    val useSrc0 = Bool()
    val useSrc1 = Bool()
    val fencei = Bool()
    val useMemory = Bool()
    val memoryOption = MemoryOp()  //load or store
    //val csr = CSR()
  }

  object InstructionType{
    def BASE = M"------------------------------11"
    def BASE_MEM = M"-------------------------0-000--" //memory option
    def BASE_MEM_L = M"--------------------------0-----"
    def BASE_MEM_S = M"--------------------------1-----"
    def BASE_AUIPC = M"-------------------------00101--"
    def BASE_LUI = M"-------------------------01101--"
    def BASE_OPX = M"-------------------------0-100--" //some Alu and shift option
    def BASE_OPX_I = M"--------------------------0-----" // Alu imm
    def BASE_OPX_SHIFT = M"------------------01------------"
    def BASE_JALR = M"-------------------------11001--"
    def BASE_JAL = M"-------------------------11011--"
    def BASE_B = M"-------------------------11000--"
    def BASE_CSR = M"-------------------------11100--"
    def BASE_CSR_W = M"------------------01------------"
    def BASE_CSR_S = M"------------------10------------"
    def BASE_CSR_C = M"------------------11------------"
    def BASE_CSR_I = M"-----------------1--------------"
    def BASE_FENCEI = M"000000000000000000010000000011--"
  }

  object InstructionCtrl{
    import InstructionType._
    def apply(instruction:Bits): InstructionCtrl = {
      val ctrl = InstructionCtrl()
      //set initial decode information
      ctrl.illegal := False
      ctrl.br := BR.N
      ctrl.jump := False
      ctrl.op0 := OP0.initial
      ctrl.op1 := OP1.initial
      ctrl.alu := ALU.initial
      ctrl.useSrc0 := False
      ctrl.useSrc1 := False
      ctrl.fencei := False
      ctrl.useMemory := False
      ctrl.memoryOption := MemoryOp.initial

      //get the decode info
      when(instruction === BASE) {
        ctrl.fencei := instruction === BASE_FENCEI
        when(instruction === BASE_MEM) {
          ctrl.illegal := True
          ctrl.op0 := OP0.RS
          ctrl.alu := ALU.ADD
          ctrl.useMemory := True
          ctrl.useSrc0 := True
          when(instruction === BASE_MEM_L) {
            ctrl.memoryOption := MemoryOp.LOAD
            ctrl.op1 := OP1.IMI
          } elsewhen (instruction === BASE_MEM_S) {
            ctrl.op1 := OP1.IMS
            ctrl.memoryOption := MemoryOp.STORE
            ctrl.useSrc1 := True
          }
        }

        when(instruction === BASE_AUIPC) {
          ctrl.illegal := True
          ctrl.op0 := OP0.IMU
          ctrl.op1 := OP1.PC
          ctrl.alu := ALU.ADD
        }
        when(instruction === BASE_LUI) {
          ctrl.illegal := True
          ctrl.op0 := OP0.IMU
          ctrl.alu := ALU.COPY
        }

        when(instruction === BASE_OPX) {
          val isShift = instruction === BASE_OPX_SHIFT
          when(instruction === BASE_OPX_I) {
            when(!isShift || (instruction === M"0-00000-------------------------" && !(instruction(30) && !instruction(14)))) {
              ctrl.illegal := True
              ctrl.op0 := OP0.RS
              ctrl.op1 := OP1.IMI
              ctrl.alu.assignFromBits((isShift && instruction(30)) ## instruction(14 downto 12))
              ctrl.useSrc0 := True
            }
          } otherwise {
            when(instruction === M"0-00000-------------------------") {
              when(instruction(30) === False || instruction(14 downto 12) === B"000" || instruction(14 downto 12) === B"101") {
                ctrl.illegal := True
                ctrl.op0 := OP0.RS
                ctrl.op1 := OP1.RS
                ctrl.alu.assignFromBits(instruction(30) ## instruction(14 downto 12)) //if use add -> 0000 -> 0
                ctrl.useSrc0 := True
                ctrl.useSrc1 := True
              }
            }
          }
        }

        when(instruction === BASE_JAL) {
          ctrl.illegal := True
          ctrl.br := BR.J
          ctrl.alu := ALU.ADD
          ctrl.op0 := OP0.IMJB
          ctrl.op1 := OP1.PC
          ctrl.jump := True
        }

        when(instruction === BASE_JALR) {
          ctrl.illegal := True
          ctrl.br := BR.JR
          ctrl.jump := True
          ctrl.op0 := OP0.RS
          ctrl.op1 := OP1.IMI
          ctrl.alu := ALU.ADD
          ctrl.useSrc0 := True
        }

        when(instruction === BASE_B) {
          ctrl.illegal := True
          ctrl.alu := ALU.ADD
          ctrl.op0 := OP0.IMJB
          ctrl.op1 := OP1.PC
          ctrl.br.assignFromBits(False ## instruction(14 downto 12))  //follow the branch order
          ctrl.useSrc0 := True
          ctrl.useSrc1 := True
        }
      }
      ctrl
    }
  }

}