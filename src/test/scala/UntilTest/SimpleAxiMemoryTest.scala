package UntilTest
import spinal.core.sim._
import common._
import org.scalatest.funsuite.AnyFunSuite
import Common.Sim.Bus._
import spinal.lib.bus.amba4.axi._
import spinal.lib.sim.ScoreboardInOrder

import scala.util.Random

class SimpleAxiMemoryTest extends AnyFunSuite {
  var compiled:SimCompiled[SimpleAxiMemory] = null
  test("compile"){
    val axiconfig = Axi4Config(
      addressWidth = 10,
      dataWidth = 32,
      useId = true,
      idWidth = 3,
      useRegion = false,
      useBurst = false,
      useLock = false,
      useQos = false
    )
    compiled = DSASimConfig().compile(new SimpleAxiMemory(axiconfig))
  }

  //test for the axi4 driver
  test("testbench"){
    compiled.doSim(seed = 42){
      dut =>
        dut.clockDomain.forkStimulus(10)
        val scoreboard = ScoreboardInOrder[BigInt]()
        //test axi driver
        var wdata = 0
        val dirver = AxiDriver(dut.io.bus,dut.clockDomain)
        dut.clockDomain.waitSampling(5)
        for(idx <- 0 until 1000){
          wdata = Random.nextInt(500)
          dirver.write(idx,wdata)
          scoreboard.pushRef(wdata)
        }
        println("write sim time: " + simTime().toString)
        for (idx <- 0 until 1000) {
          scoreboard.pushDut(dirver.read(idx))
        }
    }
  }
}
