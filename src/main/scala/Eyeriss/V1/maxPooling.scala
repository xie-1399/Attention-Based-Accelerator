package Eyeriss.V1

/* * the max pooling Unit deal with the output data
   * not support the overlapping pooling -> so the stride should equal the filter size
   */


import DefineSim.SpinalSim.PrefixComponent
import spinal.core._
import spinal.lib._

case class poolingParameters(
                            rowWidth:Int = 8,
                            colWidth:Int = 8,
                            channelNumWidth:Int = 8,
                            dataWidth:Int = 8,
                            strideWidth:Int = 4
                            )


class maxPooling(p:poolingParameters, cols:Int = 4,stride:Int = 2) extends PrefixComponent{
  assert(cols % 2 == 0)
  import p._
  val io = new Bundle{
    val rowNum = in UInt(rowWidth bits)
    val colNum = in UInt(colWidth bits)
    val dataIn = slave Stream Vec(SInt(dataWidth bits),cols)
    val transDone = in Bool()
    val dataOut = master Stream(Vec(SInt(dataWidth bits),cols / (stride * stride)))
    val done = out Bool()
  }
  /* with initial value */
  val din = io.dataIn.payload.grouped(2).toList /* each group has two elem */

  for(idx <- 0 until din.length){
  }



}
