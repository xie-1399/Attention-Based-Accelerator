package MicroRV32.core

import spinal.core._
import spinal.lib._

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

case class DecodeBundle(rvConfig: RVConfig) extends Bundle{
  val instruction = in Bits(rvConfig.xlen bits)
  //needs out?
  val fields = DecodedFields
  val immediate = out Bits(rvConfig.xlen bits)
  val csr_uimm = out Bits(5 bits)  //from the rs1 field
  val validDecode = out Bool()  // valid decode?

  val instType = out (InstructionType())
  val csrType = out(CSRAccessType())

}

class DecoderUnit(rvConfig: RVConfig) extends Component {


}
