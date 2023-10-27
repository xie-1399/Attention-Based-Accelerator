package EyerissV1

import DefineSim.{SIMCFG, VecSim}
import spinal.core.sim._
import Eyeriss.V1.{ActParameters, ReLU}
import org.scalatest.funsuite.AnyFunSuite
import DefineSim.SimUntils._

import scala.collection.mutable._
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

  test("RELU_pipe"){
    SIMCFG(gtkFirst = true).compile {
      val dut = new ReLU(ActParameters())
      dut
    }.doSim {
      dut =>
        dut.clockDomain.forkStimulus(10)
        dut.io.dataIn.valid #= false
        dut.clockDomain.waitSampling()
        val board = Queue[BigInt]()

        /* monitor the relu function */
        def monitor() = {
          for(idx <- 0 until 10000){
            dut.io.dataIn.valid.randomize()
            val random = GenRandomList(-50, 100, 32)
            VecSim.VecSInt(random, dut.io.dataIn.payload, sign = true)
            dut.clockDomain.waitSampling()

            val log = if(dut.io.dataIn.valid.toBoolean && dut.io.dataIn.ready.toBoolean) true else false
            if(log){
              print("Data:")
              random.foreach(ele => print(s"$ele" + "\t"))
              print("\n")
              for(data <- random){
                if(data > 0) board.enqueue(data)
                else board.enqueue(0)
              }
            }
            if(dut.io.dataOut.valid.toBoolean){
              // VecSim.logout(dut.io.dataOut.payload, seperate = 8)
              for(data <- dut.io.dataOut.payload) {assert(data.toBigInt == board.dequeue())}
            }
          }
        }
        monitor()
    }

  }

}
