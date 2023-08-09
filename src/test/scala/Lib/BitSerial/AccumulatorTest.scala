package Lib.BitSerial

import Lib.Math.BitSerial.{Accumulator, Adder}
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import common._

import scala.util.Random

class AccumulatorTest extends AnyFunSuite{
  var compiled:SimCompiled[Accumulator] = null
  var bitAdder:SimCompiled[Adder] = null
  test("compile"){
    compiled = DSASimConfig().compile(new Accumulator(16))
    bitAdder = DSASimConfig().compile(new Adder)
  }

  test("testbench"){
    compiled.doSim(seed = 42){
      dut =>
        dut.clockDomain.forkStimulus(10)
        for(idx <- 0 until 50){
          dut.io.value #= Random.nextInt(2)
          dut.io.signBit #= false
          dut.clockDomain.waitSampling()
        }
    }
  }

  test("Bit Adder"){
    bitAdder.doSim(seed = 42){
      dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.bit_rs1 #= 0
        dut.io.bit_rs2 #= 0
        dut.clockDomain.waitRisingEdge()

        // c = 0
        dut.io.bit_rs1 #= 1
        dut.io.bit_rs2 #= 0
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 1)

        // c = 0
        dut.io.bit_rs1 #= 1
        dut.io.bit_rs2 #= 1
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 0)

        // c = 1
        dut.io.bit_rs1 #= 1
        dut.io.bit_rs2 #= 0
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 0)

        // c = 1
        dut.io.bit_rs1 #= 0
        dut.io.bit_rs2 #= 0
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 1)

        // c = 0
        dut.io.bit_rs1 #= 1
        dut.io.bit_rs2 #= 1
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 0)

        // c = 1
        dut.io.bit_rs1 #= 1
        dut.io.bit_rs1 #= 0
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 0)

        // c = 1
        dut.io.bit_rs1 #= 0
        dut.io.bit_rs2 #= 0
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 1)

        // c = 0
        dut.io.bit_rs1 #= 0
        dut.io.bit_rs2 #= 0
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 0)
    }
  }

}
