package Eyeriss.V1

import DefineSim.SpinalSim.PrefixComponent
import DefineSim._
import spinal.core._
import spinal.lib._
import scala.collection.mutable._
/*
 * PE contains calculate the MAC between filter and weight
 * this shows very simple way to calculate the MAC with PE Array
 * calculate with the row by row
*/

class PE(filterLen:Int,fmapLen:Int,dataWidth:Int) extends PrefixComponent{
  require(filterLen <= fmapLen)
  val io = new Bundle{
    val filterRow = in(Vec(SInt(dataWidth bits),filterLen))
    val fmapRow = in(Vec(SInt(dataWidth bits),fmapLen))
    val sumIn = in(Vec(SInt(2 * dataWidth bits),fmapLen - filterLen + 1))
    val filterRowOut = out(Vec(SInt(dataWidth bits),filterLen))
    val sumOut = out(Vec(SInt(2 * dataWidth bits),fmapLen - filterLen + 1))
  }

  val localConvSum = Vec(Vec(SInt( 2 * dataWidth bits),filterLen),fmapLen - filterLen + 1)
  val tempSum = Vec(SInt(2 * dataWidth bits),fmapLen - filterLen + 1)

  /* simple set the stride is 1 */
  for(row <- 0 until fmapLen - filterLen + 1){
    for(col <- 0 until filterLen){
      localConvSum(row)(col) := io.filterRow(col) * io.fmapRow(col + row)
    }
    tempSum(row) := localConvSum(row).reduceBalancedTree(_ + _)
  }
  for(idx <- 0 until fmapLen - filterLen + 1){
    io.sumOut(idx) := tempSum(idx) + io.sumIn(idx)
  }

  io.filterRowOut := io.filterRow
}

/* set PE Array Here */

object PETest extends App {
  import spinal.core.sim._
  SIMCFG(gtkFirst = true).compile{
    val dut = new PE(3,6,8)
    dut
  }.doSim{
    dut =>
      def calculate(filter:Seq[BigInt],fmap:Seq[BigInt]) = {
        val array = ArrayBuffer[BigInt]()
        for(i <- 0 until fmap.length - filter.length + 1){
          var sum:BigInt = 0
          for(j <- 0 until filter.length){
            sum += filter(j) * fmap(i + j)
          }
          array += sum
        }
        array
      }

      dut.clockDomain.forkStimulus(10)
      for(iter <- 0 until 100){
        val filterValue = SimUntils.GenRandomList(-25, 50, 3, log = true, prefix = "filter:")
        val fmapValue = SimUntils.GenRandomList(-25, 50, 6, log = true, prefix = "fmap:")
        VecSim.VecSInt(filterValue, dut.io.filterRow, sign = true)
        VecSim.VecSInt(fmapValue, dut.io.fmapRow, sign = true)
        VecSim.VecSInt(Seq.fill(4)(BigInt(0)), dut.io.sumIn, sign = true)
        dut.clockDomain.waitSampling()
        // dut.io.sumOut.toList.foreach(item => println(item.toInt))
        val array = calculate(filterValue, fmapValue)
        for (idx <- 0 until array.length) {
          assert(array(idx) == dut.io.sumOut(idx).toBigInt)
        }
      }

  }


}