package MicroRV32.Privilige

import spinal.core._

//CSR support of the RV32

object CSRAccessType extends SpinalEnum{
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

}

