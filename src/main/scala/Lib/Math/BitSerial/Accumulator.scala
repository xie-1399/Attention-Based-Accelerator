package Lib.Math.BitSerial
import spinal.core._

class Accumulator(n:Int) extends Component {
  val io = new Bundle{
    val value = in(UInt(1 bits))
    val signBit = in(Bool()) //whether is the sign bit
    val sum = out(UInt(1 bits))
  }
  noIoPrefix()
  val data = RegInit(Vec(U(0,1 bits),n))

  val carry = RegInit(U(0,1 bits))

  val res = data(0) +^ io.value + carry
  io.sum := res(0).asUInt
  data(n-1) := res(0).asUInt

  when(io.signBit){
    carry := 0
  }.otherwise{
    carry := res(1).asUInt
  }

  for(i <- 0 until n-1){
    data(i) := data(i + 1)
  }
}

//simple bit Adder
class Adder extends Component{
  val io = new Bundle{
    val bit_rs1 = in UInt(1 bits)
    val bit_rs2 = in UInt(1 bits)

    val sum = out UInt(1 bits)
  }
  noIoPrefix()
  val carry = RegInit(U(0,1 bits))
  val res = io.bit_rs1 +^ io.bit_rs2 + carry
  io.sum := res(0).asUInt
  carry := res(1).asUInt
}