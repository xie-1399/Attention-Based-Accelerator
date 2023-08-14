package TinyCore

import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import lib.Sim.SpinalSim
import CPU.TinyCore.Stages._

import scala.util.Random

class RegfileTest extends AnyFunSuite{

  //async: get the data at once (sync one cycle late)

  var Readasync:SimCompiled[Regfile] = null
  var Readsync:SimCompiled[Regfile] = null
  test("compile"){
    Readasync = SpinalSim().compile(new Regfile())
    Readsync = SpinalSim().compile(new Regfile(setName = false,regfileReadKind = sync))
  }

  test("async or sync"){
    val async = true //change read kind
    val sim = if(async) Readasync else Readsync

    sim.doSim(seed = 42){
      dut =>
        def write(index:Int,data:BigInt) = {
          dut.io.write #= true
          dut.io.data #= data
          dut.io.rs0 #= 0
          dut.io.rs1 #= 0
          dut.io.rd #= index
          dut.clockDomain.waitSampling()
        }

        def read(index0:Int,index1:Int) = {
          dut.io.write #= false
          dut.io.rs0 #= index0
          dut.io.rs1 #= index1
          dut.io.rd #= 0
          dut.clockDomain.waitSampling()
          if(async){
            assert(dut.io.rs0Data.toBigInt == index0)
            assert(dut.io.rs1Data.toBigInt == index1)
          }
          else {
            dut.clockDomain.waitSampling()
            assert(dut.io.rs0Data.toBigInt == index0)
            assert(dut.io.rs1Data.toBigInt == index1)
          }

        }

        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling(5)
        //write data
        for(idx <- 0 until 32){
          write(idx,idx)
        }
        dut.clockDomain.waitSampling(5)
        for(idx <- 0 until 10){
          val index1 = Random.nextInt(32)
          val index0 = Random.nextInt(32)
          read(index0, index1)
        }
        dut.clockDomain.waitSampling()
    }
  }

}
