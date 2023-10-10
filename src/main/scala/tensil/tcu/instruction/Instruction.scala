package tensil.tcu.instruction

/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

import spinal.core._
import spinal.lib._
import tensil.common._
 /*
 * opcode : define the operation of the DLA
 * flags : show more information about the behaviour
 * arguments : ???
 */

case class Instruction(val instructionWidth:Int) extends Bundle {
  val opcode = UInt(4 bits)
  val flags = UInt(4 bits)
  val arguments = UInt((instructionWidth - 8) bits)
}

object Instruction{

  def apply(opcode:UInt,flags:UInt,arguments:UInt)(implicit layout:InstructionLayout):Instruction = {
    val instruction = Instruction(layout.instructionSizeBytes * 8)
    instruction.opcode := opcode
    instruction.flags := flags
    instruction.arguments := arguments.resized
    instruction
  }

  def apply(opcode: UInt, arguments: UInt)(implicit layout: InstructionLayout): Instruction = {
    val instruction = Instruction(layout.instructionSizeBytes * 8)
    instruction.opcode := opcode
    instruction.flags := U(0).resized
    instruction.arguments := arguments.resized
    instruction
  }

  def apply(opcode: UInt)(implicit layout: InstructionLayout): Instruction = {
    val instruction = Instruction(layout.instructionSizeBytes * 8)
    instruction.opcode := opcode
    instruction.flags := U(0).resized
    instruction.arguments := U(0).resized
    instruction
  }

  def fromUInt(u: UInt)(implicit layout: InstructionLayout): Instruction ={
    val width = layout.instructionSizeBytes * 8
    val instruction = Instruction(width)
    instruction.opcode := u(width - 1 downto width - 4)
    instruction.flags := u(width - 5 downto width - 8)
    instruction.opcode := u(width - 9 downto 0)
    instruction
  }
}

object Opcode {
  val NoOp        = U(0x0)
  val MatMul      = U(0x1)
  val DataMove    = U(0x2)
  val LoadWeights = U(0x3)
  val SIMD        = U(0x4)
  // unused 0x5-0xe
  val Configure = U(0xf)

  val all = Array(NoOp, MatMul, DataMove, LoadWeights, SIMD, Configure)
}