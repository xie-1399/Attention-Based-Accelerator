package MicroRV32.Privilige
import spinal.core._
import spinal.lib._
import MicroRV32.core._
//control the csr unit of the rv32

class CSRUnit(rvConfig: CoreConfig) extends Area{

  val addr = UInt(12 bits)
  val accessType = CSRAccessType()
  val enable = Bool()
  val wdata = Bits(rvConfig.xlen bits) //write data
  val rdata = Bits(rvConfig.xlen bits) //read data
  val isIllegalAccess = Bool()
  val csrimmZero = Bool()  //decoder.io.csr_uimm =/= 0
  val newFecth = Bool()   //how many instructions
  val timeInterrupt = Bool() //about irqTimer
  val rdX0 = Bool()
  val rs1X0 = Bool()
  val isCsr = Bool()
  val csrCondition = Bool()

  // rdX0 := decoder.io.fields.dest === 0
  // rs1X0 := decoder.io.fields.src1 === 0
  // csrimmZero := decoder.io.csr_uimm === 0
  // isCer := decoder.io.instType === isCSR
  csrCondition := ((rs1X0 & isCsr) | (csrimmZero & ~isCsr))

  // Machine Information Registers
  val mvendorid = Reg(Bits(32 bits)) init (0) //RO - Vendor ID
  val marchid = Reg(Bits(32 bits)) init (0) // RO - Architecture ID
  val mimpid = Reg(Bits(32 bits)) init (0) // RO - Implementation ID
  val mhartid = Reg(Bits(32 bits)) init (0) // RO - Hardware thread ID
  // Machine Trap Setup
  val mstatus = Reg(Bits(32 bits)) init (RVCSR.MSTATUS_DEFAULT) // RW - Machine status retgister
  val misa = Reg(Bits(32 bits)) init (RVCSR.genMISAValue(rvConfig.hasMulDiv, rvConfig.supportCompressed)) // RW - ISA and extensions
  val medeleg = Reg(Bits(32 bits)) init (0) // RW - Machine exception delegation register
  val mideleg = Reg(Bits(32 bits)) init (0) // RW - Machine interrupt delegation register
  val mie = Reg(Bits(32 bits)) init (0) // RW - Machine interrupt-enable register
  val mtvec = Reg(Bits(32 bits)) init (0) // RW - Machine trap-handler base address
  // Machine Trap Handling
  val mepc = Reg(Bits(32 bits)) init (0) // RW - Machine exception program counter
  val mcause = Reg(Bits(32 bits)) init (0) // RW - Machine trap cause
  val mtval = Reg(Bits(32 bits)) init (0) // RW - Machine bad address or instruction
  val mip = Reg(Bits(32 bits)) init (0) // RO - Machine interrupt pending
  val mtinst = Reg(Bits(32 bits)) init (0) // RW - Machine trap instruction (transformed)
  // Hardware Performance Monitor
  val minstret = Reg(Bits(64 bits)) init (0) // RW - Instructions retired
  val mcycle = Reg(Bits(64 bits)) init (0) // RW - Clock cycles executed

  def maskValue(oldVal:Bits,mask:Bits,newVal:Bits): Bits = {
    val writeVal = Bits(rvConfig.xlen bits)
    writeVal := (oldVal & ~mask) | (newVal & mask)
    writeVal
  }

  //csr write access
  def csrWriteAccess(reg:Bits,mask:Bits,newValue:Bits): Unit = {
    when(accessType === CSRAccessType.CSRwrite){
      reg := mask & newValue
    }.elsewhen(accessType === CSRAccessType.CSRset && !csrimmZero){
      // use |
      reg := maskValue(reg,mask,newValue)
    }.elsewhen(accessType === CSRAccessType.CSRclear && !csrimmZero){
      //use &
      reg := maskValue(reg,mask,~newValue)
    }
  }


  //csr read
  rdata := B(0,rvConfig.xlen bits)
  isIllegalAccess := False
  when(enable){
    switch(addr){
      is(RVCSR.MVENDORID_ADDR) {
        rdata := mvendorid
      }
      is(RVCSR.MARCHID_ADDR) {
        rdata := marchid
      }
      is(RVCSR.MIMPID_ADDR) {
        rdata := mimpid
      }
      is(RVCSR.MHARTID_ADDR) {
        rdata := mhartid
      }
      is(RVCSR.MSTATUS_ADDR) {
        rdata := mstatus & RVCSR.MSTATUS_READ_MASK //read mask of the mstatus
        csrWriteAccess(reg = mstatus, mask = RVCSR.DEFAULT_CSR_MASK, wdata) //all write pri
      }
      is(RVCSR.MISA_ADDR) {
        rdata := misa
      }
      is(RVCSR.MEDELEG_ADDR) {
        rdata := medeleg
        csrWriteAccess(reg = medeleg, mask = RVCSR.DEFAULT_CSR_MASK, wdata)
      }
      is(RVCSR.MIDELEG_ADDR) {
        rdata := mideleg
        csrWriteAccess(reg = mideleg, mask = RVCSR.DEFAULT_CSR_MASK, wdata)
      }
      is(RVCSR.MIE_ADDR) {
        rdata := mie & RVCSR.MIE_RW_MASK
        csrWriteAccess(reg = mie, mask = RVCSR.MIE_RW_MASK, wdata)
      }
      is(RVCSR.MTVEC_ADDR) {
        rdata := mtvec
        //Todo
        csrWriteAccess(reg = mtvec, mask = RVCSR.MTVEC_WRITE_MASK, wdata & RVCSR.MTVEC_WRITE_MASK)
      }
      is(RVCSR.MEPC_ADDR){
        rdata := mepc
        if (!rvConfig.supportCompressed) {
          csrWriteAccess(reg = mepc, mask = RVCSR.MEPC_WRITE_MASK_32, wdata & RVCSR.MEPC_WRITE_MASK_32)
        } else {
          csrWriteAccess(reg = mepc, mask = RVCSR.MEPC_WRITE_MASK_16, wdata & RVCSR.MEPC_WRITE_MASK_16)
        }
      }
      is(RVCSR.MCAUSE_ADDR) {
        rdata := mcause
        csrWriteAccess(reg = mcause, mask = RVCSR.DEFAULT_CSR_MASK, wdata)
      }
      is(RVCSR.MTVAL_ADDR) {
        rdata := mtval
        csrWriteAccess(reg = mtval, mask = RVCSR.DEFAULT_CSR_MASK, wdata)
      }
      is(RVCSR.MIP_ADDR) {
        rdata := mip
      }

      // Hardware Performance Monitor / Machine Counters/Timers (all read only)
      is(RVCSR.MCYCLE_ADDR) {
        rdata := mcycle(31 downto 0)
      }
      is(RVCSR.MINSTRET_ADDR) {
        rdata := minstret(31 downto 0)
      }
      is(RVCSR.MCYCLEH_ADDR) {
        rdata := mcycle(63 downto 32)
      }
      is(RVCSR.MINSTRETH_ADDR) {
        rdata := minstret(63 downto 32)
      }
      default {
        isIllegalAccess := True
        rdata := B(0, 32 bits)
      }
    }
  }

  when(timeInterrupt){
    mip(RVCSR.MIP_MTIP) := True
  }.otherwise{
    mip(RVCSR.MIP_MTIP) := False
  }

  when(newFecth){
    minstret := (minstret.asUInt + 1).asBits
  }
  //always increase
  mcycle := (mcycle.asUInt + 1).asBits

}
