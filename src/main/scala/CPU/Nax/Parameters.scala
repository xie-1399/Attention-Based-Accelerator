package CPU.Nax

import spinal.core._
import spinal.lib.pipeline._

//set some parameters in the pipeline

object Global extends AreaRoot{
  val PC_WIDTH = 64
  val RVC = true
  val PC = Stageable(UInt(PC_WIDTH bits))

}

object Fetch extends AreaObject {
  val FETCH_DATA_WIDTH = 64  // two ways fetch once

  def SLICE_WIDTH = if (Global.RVC) 16 else 32
  def SLICE_BYTES = if (Global.RVC) 2 else 4
  def SLICE_RANGE_LOW = if (Global.RVC) 1 else 2

  def SLICE_COUNT = FETCH_DATA_WIDTH / SLICE_WIDTH
  def SLICE_RANGE = (SLICE_RANGE_LOW + log2Up(SLICE_COUNT) - 1) downto SLICE_RANGE_LOW  //support rvc -> 2 down to 1 else 2 down to 2

  val FETCH_PC = Stageable(Global.PC)
  val FETCH_PC_INC  = Stageable(Global.PC)

}