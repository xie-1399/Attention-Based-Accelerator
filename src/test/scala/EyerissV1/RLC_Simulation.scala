package EyerissV1

import DefineSim.{SIMCFG, VecSim}
import Eyeriss.V1.RLC.{Encoder, RLCEncoderParameters}
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import scala.collection.mutable.Seq

class RLC_Simulation extends AnyFunSuite {
  test("RLC_Encoder") {
    SIMCFG(gtkFirst = true).compile {
      val dut = new Encoder(RLCEncoderParameters(), 11)
      dut
    }.doSim {
      dut =>
        dut.clockDomain.forkStimulus(10)

        /* set the simple value */
        def comperss(values:Seq[BigInt]) = {
          dut.io.ofMap.valid #= true
          VecSim.VecUInt(values = values,vec = dut.io.ofMap.payload)
          dut.clockDomain.waitSamplingWhere(dut.io.outMap.valid.toBoolean)
          println(dut.io.outMap.payload.runPairs(0).toBigInt)
          println(dut.io.outMap.payload.levelPairs(0).toBigInt)
          println(dut.io.outMap.payload.runPairs(1).toBigInt)
          println(dut.io.outMap.payload.levelPairs(1).toBigInt)
          println(dut.io.outMap.payload.runPairs(2).toBigInt)
          println(dut.io.outMap.payload.levelPairs(2).toBigInt)
          println(dut.io.outMap.payload.term.toBigInt)
          dut.io.ofMap.valid #= false
          dut.clockDomain.waitSampling()
        }
         comperss(Seq(0,0,12,0,0,0,0,53,0,0,22))
    }
  }
}

