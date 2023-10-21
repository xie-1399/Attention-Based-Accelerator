package EyerissV1

import DefineSim.{SIMCFG, VecSim}
import spinal.core.sim._
import Eyeriss.V1.{ActParameters, ReLU}
import org.scalatest.funsuite.AnyFunSuite
import DefineSim.SimUntils._
import scala.collection.mutable.Seq

class RELU_Simulation extends AnyFunSuite {
  test("RELU") {
    SIMCFG(gtkFirst = true).compile {
      val dut = new ReLU(ActParameters(pipe = false))
      dut

    }.doSim {
      dut =>
        dut.clockDomain.forkStimulus(10)
        val random = GenRandomList(-50,100,32,true,prefix = "simulation:")

        /* monitor the relu function */
        def monitor() = {
          dut.io.dataIn.valid #= true
          VecSim.VecSInt(random,dut.io.dataIn.payload,sign = true)
          dut.clockDomain.waitSampling()
          VecSim.logout(dut.io.dataOut.payload,seperate = 8)
          for(idx <- 0 until random.length){
            val real = if(random(idx) >=0) random(idx) else 0
            assert(real == dut.io.dataOut.payload(idx).toBigInt)
          }
        }
        monitor()

    }
  }
}
