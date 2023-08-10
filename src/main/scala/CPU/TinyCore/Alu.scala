package CPU.TinyCore

//Alu Unit implement ADD SUB AND OR XOR SLT(rs1 < rs2 ? 1:0) SLTU

import spinal.core._
import spinal.lib._
import Misc._

class Alu extends Component {
  import Global._
  import ALU._
  val io  = new Bundle{
    val func = in(ALU) //the Alu is the spinal enum type
    val doSub = in Bool()
    val src0 = in Bits(Xlen bits)
    val src1 = in Bits(Xlen bits)
    val result = out Bits(Xlen bits)
    val adder = out UInt(Xlen bits)
    val actual = out SInt(Xlen bits)
  }
  noIoPrefix()
  //add and sub(or just divide)
  val addSub = (io.src0.asSInt + Mux(io.doSub, ~io.src1, io.src1).asSInt + Mux(io.doSub,S(1),S(0))).asBits //notice not S"0"

  //if just want match some types(logic)
  val bitwise = io.func.mux(
    AND -> (io.src1 & io.src0),
    OR -> (io.src0 | io.src1),
    XOR -> (io.src0 ^ io.src1),
    default -> io.src0    //just copy
  )

  //SLT SLTU
  val less = Mux(io.src0.asSInt < io.src1.asSInt , B"1", B"0") //Todo with msb


  //get results
  io.result := io.func.mux(
    (ADD,SUB) -> addSub,
    (SLT,SLTU) -> less.asBits.resized,
    default -> bitwise
  )
  io.adder := addSub.asUInt.resized
  io.actual := addSub.asSInt.resized
}
