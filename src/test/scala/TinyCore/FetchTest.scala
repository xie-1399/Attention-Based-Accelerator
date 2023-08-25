package TinyCore

//test the fetch using pc
import CPU.TinyCore.RiscvCoreConfig
import CPU.TinyCore.Sim._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import lib.Sim.SpinalSim._
import lib.Sim._
import lib.Sim.StramFlowSim._
import CPU.TinyCore.Stages._
import spinal.core.Component.push
import spinal.lib.sim.StreamReadyRandomizer

import scala.util.Random

class FetchTest extends AnyFunSuite{
  var compiled:SimCompiled[FetchMemory] = null
  test("compile"){
    val config = RiscvCoreConfig(addressWidth = 6,pcWidth = 6,startAddress = 0x00000000)
    compiled = SpinalSim().compile{
      val dut = new FetchMemory()(config)
      addSimPublic(List(dut.fetch.io.haltCpu,dut.fetch.io.pcLoad))
      dut
    }
  }

  test("only the fetch") {
    SIMCFG(gtkFirst = true).compile {
      val config = RiscvCoreConfig()
      val dut = new Fetch()(config)
      addSimPublic(List(dut.Fetch.flush,dut.Fetch.throwIt))
      dut
    }.doSim {
      dut =>
        dut.clockDomain.forkStimulus(10)
        var request = 0
        def noJump() = {
          dut.io.haltCpu #= false
          dut.io.pcLoad.valid #= false
          dut.io.iRsp.valid #= false
          dut.io.iRsp.ready #= false
          if(dut.io.iCmd.valid.toBoolean && dut.io.iCmd.ready.toBoolean){
            assert(dut.io.iCmd.pc.toLong == 0x80000000l + request * 4)
            request += 1
          }
          if(request == 3){
            //test flush the fetch stage
            dut.Fetch.throwIt #= true
            //set some response
            dut.io.iRsp.valid #= true
            dut.io.iRsp.ready #= true
            dut.io.iRsp.instruction #= 0x00000013
            dut.io.iRsp.pc #= 0x80000000l + Random.nextInt(5) * 4  //long use
          }

        }
        //the pcLoad will change the pcNext value at once
        def withJump() = {
          val randomPc = 0x90000000l + Random.nextInt(100) * 4
          val randomGen = Random.nextInt(10) > 5
          dut.io.haltCpu #= false
          dut.io.pcLoad.valid #= false
          dut.io.iRsp.valid #= false
          dut.io.iRsp.ready #= false
          if(randomGen){
            dut.io.pcLoad.valid #= true
            dut.io.pcLoad.payload #= randomPc
            dut.clockDomain.waitSampling()
            if (dut.io.iCmd.valid.toBoolean && dut.io.iCmd.ready.toBoolean) {
              assert(dut.io.iCmd.pc.toLong == randomPc)
            }
          }
        }
        StreamReadyRandomizer(dut.io.iCmd,dut.clockDomain)
        //onlySample(dut.clockDomain,operation = noJump,iter = 30)
        onlySample(dut.clockDomain,operation = withJump , iter = 30)
    }
  }

  test("fetch with the bus request"){
    //Todo get the memory data
  }

}
