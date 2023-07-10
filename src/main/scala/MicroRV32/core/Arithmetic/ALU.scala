package MicroRV32.core.Arithmetic
import spinal.core._
import spinal.lib._
import MicroRV32.core._
import MicroRV32.core.const._
import Common.spinalConfig._
//the Alu arithmetic
case class ALUInstruction() extends Bundle{
  val funct3 = in Bits(3 bits)
  val funct7 = in Bits(7 bits)
  val shamt = in Bits(5 bits)
  val iType = InstructionType()
}

case class ALUBundle(rvConfig: CoreConfig) extends Bundle{
  val opA = in Bits(rvConfig.xlen bits)
  val opB = in Bits(rvConfig.xlen bits)
  val operation = in(ALUInstruction())
  val output = out Bits(rvConfig.xlen bits)
  val outputBool = out Bool()  // out compare signal
}

class ALU(rvConfig: CoreConfig) extends Component {
  val xlen = rvConfig.xlen
  val io = ALUBundle(rvConfig)
  noIoPrefix()
  //compare results signal
  val equal = Bool()
  val unequal = Bool()
  val lt_u = Bool()
  val lt_s = Bool()
  val ge_u = Bool()
  val ge_s = Bool()
  //arith results signal
  val add = Bits(xlen bits)
  val sub = Bits(xlen bits)
  val bitAnd = Bits(xlen bits)
  val bitOr = Bits(xlen bits)
  val bitXor = Bits(xlen bits)
  val shiftL = Bits(xlen bits)
  val shiftR = Bits(xlen bits)
  val shiftRA = Bits(xlen bits)
  val shiftLI = Bits(xlen bits)
  val shiftRI = Bits(xlen bits)
  val shiftRAI = Bits(xlen bits)

  //Todo how to deal the overflow?
  add := (io.opA.asUInt + io.opB.asUInt).asBits
  sub := (io.opA.asUInt - io.opB.asUInt).asBits
  equal := io.opA === io.opB
  unequal := !equal
  lt_u := io.opA.asUInt < io.opB.asUInt
  lt_s := io.opA.asSInt < io.opB.asSInt
  ge_u := io.opA.asUInt >= io.opB.asUInt
  ge_s := io.opA.asSInt >= io.opB.asSInt
  bitAnd := io.opA & io.opB
  bitOr := io.opA | io.opB
  bitXor := io.opA ^ io.opB
  //use logic shift
  shiftL := io.opA |<< io.opB(4 downto 0).asUInt
  shiftR := io.opA |>> io.opB(4 downto 0).asUInt
  shiftLI := io.opA |<< io.operation.shamt.asUInt
  shiftRI := io.opA |>> io.operation.shamt.asUInt
  //arithmetic shift（only the right shifter）
  shiftRA := (io.opA.asSInt >> io.opB(4 downto 0).asUInt).asBits
  shiftRAI := (io.opA.asSInt >> io.operation.shamt.asUInt).asBits

  io.output := 0
  io.outputBool := False

  //allow multi dup
  val ir = InstructionType
  val rv = RV32Fields
  switch(io.operation.iType , strict = false){
    is(ir.isCT_JAL, ir.isCT_JALR, ir.isStore, ir.isLoad, ir.isLUI, ir.isAUIPC) {
      io.output := add
    }
    is(ir.isRType){
      switch(io.operation.funct3,strict = false){
        is(rv.F3_ADD, rv.F3_SUB) {
          io.output := (io.operation.funct7 === rv.F7_Z) ? add | sub
        }
        is(rv.F3_SLT) {
          io.output := (lt_s === True) ? B(1, 32 bits) | B(0, 32 bits)
        }
        is(rv.F3_SLTU) {
          io.output := (lt_u === True) ? B(1, 32 bits) | B(0, 32 bits)
        }
        is(rv.F3_AND) {
          io.output := bitAnd
        }
        is(rv.F3_OR) {
          io.output := bitOr
        }
        is(rv.F3_XOR) {
          io.output := bitXor
        }
        is(rv.F3_SLL) {
          io.output := shiftL
        }
        is(rv.F3_SRL, rv.F3_SRA) {
          io.output := (io.operation.funct7 === rv.F7_Z) ? shiftR | shiftRA
        }
      }
    }
    is(ir.isRImm){
      switch(io.operation.funct3,strict = false){
        is(rv.F3_ADDI) {
          io.output := add
        }
        is(rv.F3_SLTI) {
          io.output := (lt_s === True) ? B(1, 32 bits) | B(0, 32 bits)
        }
        is(rv.F3_SLTIU) {
          io.output := (lt_u === True) ? B(1, 32 bits) | B(0, 32 bits)
        }
        is(rv.F3_ANDI) {
          io.output := bitAnd
        }
        is(rv.F3_ORI) {
          io.output := bitOr
        }
        is(rv.F3_XORI) {
          io.output := bitXor
        }
        is(rv.F3_SLLI) {
          io.output := shiftLI
        }
        is(rv.F3_SRLI, rv.F3_SRAI) {
          io.output := (io.operation.funct7 === rv.F7_Z) ? shiftRI | shiftRAI
        }
      }
    }

    is(ir.isBranch){
      switch(io.operation.funct3){
        is(rv.F3_BEQ) {
          io.outputBool := equal
        }
        is(rv.F3_BNE) {
          io.outputBool := unequal
        }
        is(rv.F3_BLT) {
          io.outputBool := lt_s
        }
        is(rv.F3_BGE) {
          io.outputBool := ge_s
        }
        is(rv.F3_BLTU) {
          io.outputBool := lt_u
        }
        is(rv.F3_BGEU) {
          io.outputBool := ge_u
        }
      }
    }

  }

}