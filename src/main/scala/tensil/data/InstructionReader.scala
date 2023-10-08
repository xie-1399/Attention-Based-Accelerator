package tensil.data

/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

import java.io.InputStream


 /*
  just define a class to read the instruction for iteration
 */
class InstructionReader(instructionStream:InputStream,instructionSizeBytes: Int){

  val readLength = instructionSizeBytes
  private val bytes = new Array[Byte](readLength)
  private var lastRead:Int = instructionStream.read(bytes,0,readLength) /* keep the last read instruction bytes */

  def hasNext:Boolean = {
    lastRead != -1
  }

  def next():BigInt = {
    val v = BigInt(bytes.reverse)
    lastRead = instructionStream.read(bytes, 0, readLength)
    v
  }
}
