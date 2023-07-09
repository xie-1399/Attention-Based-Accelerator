package MicroRV32.core
import spinal.core._
//the part of rv32 to generate cpu core

case class CoreConfig(resetVector:Long = 0x80000000l,
                          formalInterface:Boolean = false, //Todo
                          genMul:Boolean = true,
                          genDiv:Boolean = true,
                          genCompressed:Boolean = false,
                          genCsr:Boolean = true,
                          debugPort : Boolean = true,
                          Xlen:Int = 32,
                          withCompressed:Boolean = false){
  def supportDebug = debugPort
  def supportCsr = genCsr
  def supportCompressed = genCompressed
  def supportMul = genMul
  def supportDiv = genDiv
  def hasMulDiv = supportMul | supportDiv
  //no div without the mul
  assert((genMul & !genDiv) | (genMul & genDiv) | (!genDiv & !genMul),
    message = "the Div unit can't support without the Mul")

  val xlen = Xlen
  val compressExtension = withCompressed
  val regfileAddressWidth = 5 //32 regs
}

object spinalConfig{
  val setconfig = SpinalConfig(mode = Verilog,targetDirectory = "./rtl")
}

object InstructionType extends SpinalEnum{
  // RV32I
  val isUndef, isRType, isRImm, isImm, isBranch, isLoad, isStore,
  isCT_JAL, isCT_JALR, isLUI, isAUIPC, isECall, isFence, isIllegal,
  // CSR
  isCSR, isCSRImm, isTrapReturn,
  // MUL DIV REM
  isMulDiv = newElement()
}

//generator the core here
