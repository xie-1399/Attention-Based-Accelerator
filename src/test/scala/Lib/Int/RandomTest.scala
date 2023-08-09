package Lib.Int

import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import Lib.Math.Int.RandomGenerator
import common._

import scala.util.Random

class RandomTest extends AnyFunSuite{
  var compiled:SimCompiled[RandomGenerator] = null
  test("compile"){
    compiled = DSASimConfig().compile(new RandomGenerator(32))
  }

  test("testbench"){
    compiled.doSim(seed = 42){
      dut =>
        dut.clockDomain.forkStimulus(10)
        for(idx <- 0 until 20){
          dut.io.valid #= Random.nextInt(5) > 2
          dut.clockDomain.waitSampling()
        }
    }
  }
}
