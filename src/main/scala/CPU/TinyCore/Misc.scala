package CPU.TinyCore
import spinal.core._

object Misc {
  object Global{
    def Xlen = 32
  }

  //set the Alu type
  object ALU extends SpinalEnum{
    val ADD,SLL,SLT,SLTU,XOR,SRL,OR,AND,SUB,SRA,COPY = newElement()

    defaultEncoding = SpinalEnumEncoding("alu")(
      ADD -> 0,
      SLL -> 1,
      SLT -> 2,
      SLTU -> 3,
      XOR -> 4,
      SRL -> 5,
      OR -> 6,
      AND -> 7,
      SUB -> 8,
      SRA -> 9,
      COPY -> 15,
    )
    def X = ADD
    def isSltX(that:C) = that.asBits === M"-01-" //slt or sltu
    def isAddSub(that:C) = that.asBits === M"-000" // add or sub
  }
}
