package CPU.MicroRV32.core

import spinal.core._
import scala.math._
import Common.spinalConfig._
// the rggfile of the RV core

case class RegFileIO(addressWidth:Int,dataWidth:Int) extends Bundle{
  val rs1 = in UInt(addressWidth bits)
  val rs2 = in UInt(addressWidth bits)
  val rs1Data = out Bits(dataWidth bits)
  val rs2Data = out Bits(dataWidth bits)
  val wr = in Bool()
  val rd = in UInt(addressWidth bits)
  val rdData = in Bits(dataWidth bits)
}


class Regfile(addressWidth:Int,dataWidth:Int) extends Component {
  val io = new RegFileIO(addressWidth, dataWidth)
  val wordCount = pow(2,addressWidth).toInt//regfile depth
  val regFile =  Mem(Bits(dataWidth bits),wordCount)

  //init the mem value
  regFile.init(List.fill(wordCount)(B(0,dataWidth bits)))
  io.rs1Data := regFile.readSync(io.rs1)
  io.rs2Data := regFile.readSync(io.rs2)
  //keep r0 = 0
  regFile.write(io.rd,io.rdData,enable = io.wr && io.rd =/= 0)
}
