package TinyCore

//monitor the instruction cache about the flush/read/fill

import CPU.TinyCore.Memory._
import CPU.TinyCore.Misc.Config.instructionCacheConfig
import CPU.TinyCore.RiscvCoreConfig
import CPU.TinyCore.Sim._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import lib.Sim.SpinalSim
import lib.Sim._
import lib.Sim.StramFlowSim._


class InstructionCacheTest extends AnyFunSuite{
  var compiled:SimCompiled[InstructionCache] = null
  test("compile"){
    compiled = SpinalSim().compile{
      val dut = new InstructionCache()(instructionCacheConfig)
      dut
    }
  }


  //use the  Axi Memorysim
//  test("testBench"){
//    //first get the mem data / then cpu fetch the data / last test the flush bus
//
//    compiled.doSim(seed = 42){
//      dut =>
//        dut.clockDomain.forkStimulus(10)
//
//        def setMemBusValue(payload: InstructionCacheMemCmd): Boolean = {
//
//          payload.address #=
//
//          if (dut.io.instructionSignal.cmd.valid.toBoolean && dut.io.instructionSignal.cmd.ready.toBoolean) {
//            number += 1
//          }
//          true
//        }






//
//    }
//  }





}
