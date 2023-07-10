package CPU.MicroRV32.core

import spinal.core._


//get the imm value of the instruction
case class immBundle(immWidth:Int = 32,CsrimmWidth:Int = 5) extends Bundle{
  val instruction = in Bits(const.instructionWidth bits)
  val i_imm = out Bits(immWidth bits)
  val j_imm = out Bits(immWidth bits)
  val s_imm = out Bits(immWidth bits)
  val b_imm = out Bits(immWidth bits)
  val u_imm = out Bits(immWidth bits)
  val csr_imm = out Bits(CsrimmWidth bits)
}

class ImmExtensionUnit extends Component{
  val io = new immBundle()
  //let imm 32 bits
  // I-Type :
  io.i_imm := S(io.instruction(31), 21 bits) ## io.instruction(30 downto 25) ## io.instruction(24 downto 21) ## io.instruction(20)
  // J-Type :
  io.j_imm := S(B(io.instruction(31) ## io.instruction(19 downto 12) ## io.instruction(20) ## io.instruction(30 downto 25) ## io.instruction(24 downto 21) ## B(0, 1 bit)), 32 bits).asBits
  // S-Type :
  io.s_imm := S(io.instruction(31), 21 bits) ## io.instruction(30 downto 25) ## io.instruction(11 downto 8) ## io.instruction(7)
  // B-Type :
  io.b_imm := S(io.instruction(31), 20 bits) ## io.instruction(7) ## io.instruction(30 downto 25) ## io.instruction(11 downto 8) ## B(0, 1 bit)
  // U-Type :
  // LUI : move the imm to the rd (31:12) and add 0 at last
  // AUIPC : add with the pc(create control address)
  io.u_imm := io.instruction(31) ## io.instruction(30 downto 20) ## io.instruction(19 downto 12) ## B(0, 12 bits)
  // CSR-Type
  io.csr_imm := io.instruction(19 downto 15)
}
