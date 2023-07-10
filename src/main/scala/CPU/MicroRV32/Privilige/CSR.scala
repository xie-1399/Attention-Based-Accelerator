package CPU.MicroRV32.Privilige

import spinal.core._


/*
CSR support of the RV32
csrrw: such as csrrw rd,csr,rs1 (read a value from csr to rd and write the rs1 data to the csr)
csrrs: such as csrrs rd,csr,rs1 (let rs1 value | csr value)
csrrc: (let rs1 value | csr value)
csrrwi: write a imm value
csrrsi: imm | csr value
csrrci: imm & csr value
*/

object CSRAccessType extends SpinalEnum{
  //know about the set and clear
  val CSRidle, CSRread, CSRwrite, CSRset, CSRclear = newElement()
}

object CSROpcode{
  //func7 -> csr address (27:20) read or write(31:30) privileged(29:28)
  def OP_CSR = M"1110011"
  def F3_CSRRW  = M"001"
  def F3_CSRRS  = M"010"
  def F3_CSRRC  = M"011"
  def F3_CSRRWI = M"101"
  def F3_CSRRSI = M"110"
  def F3_CSRRCI = M"111"
  def F3_CSR_DECODEMASK = M"-00"
  def F12_MRET = M"001100000010"
}

object RVCSR{
  //default values
  def MISA_DEFAULT = B(32 bits, (31 downto 30) -> B"01", 8 -> true, default -> false)
  def MSTATUS_DEFAULT = B(32 bits, (12 downto 11) -> B"11", default -> false) //recover the privileged of M


  //Zicer CSR Extension and set your define csr here
  def MVENDORID_ADDR = U"xF11"
  def MARCHID_ADDR = U"xF12"
  def MIMPID_ADDR = U"xF13"
  def MHARTID_ADDR = U"xF14"
  // Machine Trap Setup
  def MSTATUS_ADDR = U"x300"
  def MISA_ADDR = U"x301"
  def MEDELEG_ADDR = U"x302"
  def MIDELEG_ADDR = U"x303"
  def MIE_ADDR = U"x304"
  def MTVEC_ADDR = U"x305"
  // Machine Trap Handling
  def MEPC_ADDR = U"x341"
  def MCAUSE_ADDR = U"x342"
  def MTVAL_ADDR = U"x343"
  def MIP_ADDR = U"x344"
  // Hardware Performance Monitor / Machine Counters/Timers
  def MCYCLE_ADDR = U"xB00"
  def MINSTRET_ADDR = U"xB02"
  def MCYCLEH_ADDR = U"xB80"
  def MINSTRETH_ADDR = U"xB82"

  // mstatus access bits
  def MSTATUS_MIE = 3
  def MSTATUS_MPIE = 7
  // enable/pending interrupt access positions
  def MIP_MTIP = 7

  //set mask value of read or write csr
  def DEFAULT_CSR_MASK = B"1111_1111_1111_1111_1111_1111_1111_1111"
  def MSTATUS_READ_MASK = B"0000_0000_0000_0000_0001_1000_1000_1000"
  def MSTATUS_WRITE_MASK = B"0000_0000_0000_0000_0000_0000_1000_1000"
  def MIE_RW_MASK = B"0000_0000_0000_0000_0000_1000_1000_1000"
  def MTVEC_WRITE_MASK = B"1111_1111_1111_1111_1111_1111_1111_1100"
  def MIP_RW_MASK = B"0000_0000_0000_0000_0000_1000_1000_1000"
  def MEPC_WRITE_MASK_32 = B"1111_1111_1111_1111_1111_1111_1111_1100"
  def MEPC_WRITE_MASK_16 = B"1111_1111_1111_1111_1111_1111_1111_1110"

  //set init value of misa csr
  def genMISAValue(MulDiv:Boolean,compressed:Boolean): Bits = {
    //support RV32I
    var misaVal = (1 << 30) | (1 << 7)
    if(MulDiv){
      misaVal |= (1 << 11)
    }
    if(compressed){
      misaVal |= (1 << 1)
    }
    B(misaVal,32 bits)
  }

}

