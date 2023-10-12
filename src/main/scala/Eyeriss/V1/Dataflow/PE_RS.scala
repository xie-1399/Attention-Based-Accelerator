package Eyeriss.V1.Dataflow

import DefineSim.SpinalSim.PrefixComponent
import DefineSim._
import Utils.Conv._
import spinal.core._
import spinal.lib._

import scala.collection.mutable._

/*
 * PE contains calculate the MAC between filter and weight
 * this shows very simple way to calculate the MAC with PE Array
 * calculate with the row by row
 * the dataflow is RS and the eyeriss is also RS like
*/

class SimplePE(filterLen:Int,fmapLen:Int,dataWidth:Int) extends PrefixComponent{
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

/* set PE Array Here and can be set with pipe line
* because each cycle can calculate one line -> the cycle is enough can be out*/

class SimplePEArray (filterRowNum:Int,fmapRowNum:Int,
                     filterLen:Int,fmapLen:Int,dataWidth:Int,passFmap:Boolean = true) extends PrefixComponent{
  require(fmapRowNum >= filterRowNum)
  val io = new Bundle{
    val filterIn = in Vec(Vec(SInt(dataWidth bits),filterLen),filterRowNum)
    val fmapIn = in Vec(Vec(SInt(dataWidth bits),fmapLen),fmapRowNum)
    val sum = out(Vec(Vec(SInt(2 * dataWidth bits),fmapLen - filterLen + 1),fmapRowNum - filterRowNum + 1))
  }

  val PEs = Array.fill(filterRowNum,fmapRowNum - filterRowNum + 1){   /* create the PE Array using this way */
    new SimplePE(filterLen, fmapLen, dataWidth)
  }

  /* pass the filter and fmap in */
  for(idx <- 0 until filterRowNum){
    PEs(idx)(0).io.filterRow := io.filterIn(idx)
  }

  /* connect the psum */
  PEs(0).map(_.io.sumIn.map(_ := 0))
  for(row <- 1 until filterRowNum){
    for(col <- 0 until fmapRowNum - filterRowNum + 1){
      PEs(row)(col).io.sumIn := PEs(row - 1)(col).io.sumOut
    }
  }
  (io.sum,PEs(filterRowNum -1)).zipped.map(_ := _.io.sumOut)

  /* the filter will pass from left to right and let the feature map in*/
  for(row <- 0 until filterRowNum){
    for(col <- 1 until fmapRowNum - filterRowNum + 1){
      PEs(row)(col).io.filterRow := PEs(row)(col - 1).io.filterRowOut
    }
  }
 /* let the feature map in */
  if(passFmap){
    for (idx <- 0 until fmapRowNum - filterRowNum + 1) {
      PEs(0)(idx).io.fmapRow := io.fmapIn(idx)
    }
    for (idx <- 1 until filterRowNum) {
      PEs(idx)(fmapRowNum - filterRowNum).io.fmapRow := io.fmapIn(filterRowNum + idx - 1)
    }
    for (row <- 1 until filterRowNum) {
      for (col <- 0 until fmapRowNum - filterRowNum) {
        PEs(row)(fmapRowNum - filterRowNum - 1 - col).io.fmapRow := PEs(row - 1)(fmapRowNum - filterRowNum - col).io.fmapRow
      }
    }
  }
  else{
    for (row <- 0 until filterRowNum) {
      for (col <- 0 until fmapRowNum - filterRowNum + 1) {
        (PEs(row)(col).io.fmapRow, io.fmapIn(row + col)).zipped.map(_ := _)
      }
    }
  }

}

/* Todo Merge the simulation to the test*/

object PETest extends App {
  import spinal.core.sim._
  SIMCFG(gtkFirst = true).compile{
    val dut = new SimplePE(3,6,8)
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

object PEArrayTest extends App{
  import spinal.core.sim._
  SIMCFG(gtkFirst = true).compile {
    /* 3 * 3 filter and 5 * 5 feature map */
    val dut = new SimplePEArray(3, 5, 3,fmapLen = 5,dataWidth = 8)
    dut
  }.doSim {
    dut =>
      dut.clockDomain.forkStimulus(10)
      val filterArray = ArrayBuffer[Array[BigInt]]()
      val filterNum1 = SimUntils.GenRandomList(-25, 50, 3, log = true, prefix = "filterNum1:")
      val filterNum2 = SimUntils.GenRandomList(-25, 50, 3, log = true, prefix = "filterNum2:")
      val filterNum3 = SimUntils.GenRandomList(-25, 50, 3, log = true, prefix = "filterNum3:")
      filterArray += filterNum1.toArray
      filterArray += filterNum2.toArray
      filterArray += filterNum3.toArray
      val filterFeatureMap = ArrayBuffer[Array[BigInt]]()
      val fmap1 = SimUntils.GenRandomList(-25, 50, 5, log = true, prefix = "filtermap1:")
      val fmap2 = SimUntils.GenRandomList(-25, 50, 5, log = true, prefix = "filtermap2:")
      val fmap3 = SimUntils.GenRandomList(-25, 50, 5, log = true, prefix = "filtermap3:")
      val fmap4 = SimUntils.GenRandomList(-25, 50, 5, log = true, prefix = "filtermap4:")
      val fmap5 = SimUntils.GenRandomList(-25, 50, 5, log = true, prefix = "filtermap5:")
      filterFeatureMap += fmap1.toArray
      filterFeatureMap += fmap2.toArray
      filterFeatureMap += fmap3.toArray
      filterFeatureMap += fmap4.toArray
      filterFeatureMap += fmap5.toArray

      VecSim.VecSInt(filterNum1, dut.io.filterIn(0), sign = true)
      VecSim.VecSInt(filterNum2, dut.io.filterIn(1), sign = true)
      VecSim.VecSInt(filterNum3, dut.io.filterIn(2), sign = true)
      VecSim.VecSInt(fmap1, dut.io.fmapIn(0), sign = true)
      VecSim.VecSInt(fmap2, dut.io.fmapIn(1), sign = true)
      VecSim.VecSInt(fmap3, dut.io.fmapIn(2), sign = true)
      VecSim.VecSInt(fmap4, dut.io.fmapIn(3), sign = true)
      VecSim.VecSInt(fmap5, dut.io.fmapIn(4), sign = true)

      dut.clockDomain.waitSampling()
      val res = convCalculate(filterArray.toArray,filterFeatureMap.toArray)

      println("PE Array results: ")
      for(i <- 0 until 3){
        for(j <- 0 until 3){
          assert(dut.io.sum(i)(j).toBigInt == res(i)(j))
          print(dut.io.sum(i)(j).toBigInt + " ")
        }
        println()
      }

  }
}