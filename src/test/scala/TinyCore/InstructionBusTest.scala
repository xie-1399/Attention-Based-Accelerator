package TinyCore

//test about the InstructionBus (no branch prediction)
import CPU.TinyCore.RiscvCoreConfig
import CPU.TinyCore.Stages._
import CPU.TinyCore.Sim._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import lib.Sim.SpinalSim
import lib.Sim._
import lib.Sim.StramFlowSim._

import scala.util.Random
class InstructionBusTest extends AnyFunSuite{
  var compiled:SimCompiled[InstructionBusMemory] = null
  test("compile"){
    val config = RiscvCoreConfig(addressWidth = 6)
    compiled = SpinalSim().compile{
      val dut = new InstructionBusMemory(config)
      dut.iBus.simPublic()
      dut
    }
  }

  test("testbench"){
    compiled.doSim(seed = 42){
      dut =>
        var number = 0
        val scoreboardInOrder = ScoreBoardSimulation[BigInt]()
        dut.clockDomain.forkStimulus(10)

        //use the Stream test
        def setValue(payload: CoreInstructionCmd):Boolean = {
          dut.io.instructionSignal.rsp.ready #= true
          payload.pc #= Random.nextInt(15) * 4
          if (dut.io.instructionSignal.cmd.valid.toBoolean && dut.io.instructionSignal.cmd.ready.toBoolean) {
            number += 1
          }
          true
        }

        //only when fire push ref
        def MonitorCmd(payload:CoreInstructionCmd):Unit = {
          if(dut.io.instructionSignal.cmd.valid.toBoolean && dut.io.instructionSignal.cmd.ready.toBoolean){
            scoreboardInOrder.pushRef(payload.pc.toBigInt)
            println("ref")
          }
        }

        def MonitorRsp(payload:CoreInstructionRsp):Unit = {
          if(dut.iBus.r.valid.toBoolean && dut.iBus.r.valid.toBoolean){
            scoreboardInOrder.pushDut(payload.instruction.toBigInt)
            println("dut")
          }
        }

        StreamSimulation(dut.io.instructionSignal.cmd,useDriver = true,setValue = setValue,clockDomain = dut.clockDomain)
        StreamSimulation(dut.io.instructionSignal.cmd,useMonitor = true, monitor = MonitorCmd,clockDomain = dut.clockDomain)
        StreamSimulation(dut.io.instructionSignal.rsp,useMonitor = true, monitor = MonitorRsp,clockDomain = dut.clockDomain)
        dut.clockDomain.waitActiveEdgeWhere(scoreboardInOrder.matches == 100)
    }
  }
}
