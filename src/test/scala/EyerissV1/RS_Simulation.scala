package EyerissV1

import Eyeriss.V1.Dataflow._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import DefineSim._
import scala.collection.mutable._
import Utils._
/* the simple simulation of rs simulation*/

class RS_Simulation extends AnyFunSuite{
  test("RS_PE"){
    SIMCFG(gtkFirst = true).compile {
      val dut = new SimplePE(3, 6, 8)
      dut
    }.doSim {
      dut =>
        def calculate(filter: Seq[BigInt], fmap: Seq[BigInt]) = {
          val array = ArrayBuffer[BigInt]()
          for (i <- 0 until fmap.length - filter.length + 1) {
            var sum: BigInt = 0
            for (j <- 0 until filter.length) {
              sum += filter(j) * fmap(i + j)
            }
            array += sum
          }
          array
        }

        dut.clockDomain.forkStimulus(10)
        for (iter <- 0 until 100) {
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

  test("RS_PEArray") {
    SIMCFG(gtkFirst = true).compile {
      /* 3 * 3 filter and 5 * 5 feature map */
      val dut = new SimplePEArray(3, 5, 3, fmapLen = 5, dataWidth = 8)
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
        val res = Conv.convCalculate(filterArray.toArray, filterFeatureMap.toArray)

        println("PE Array results: ")
        for (i <- 0 until 3) {
          for (j <- 0 until 3) {
            assert(dut.io.sum(i)(j).toBigInt == res(i)(j))
            print(dut.io.sum(i)(j).toBigInt + " ")
          }
          println()
        }
    }
  }
}
