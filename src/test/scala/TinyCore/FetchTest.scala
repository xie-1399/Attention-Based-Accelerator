package TinyCore

//test the fetch using pc
import CPU.TinyCore.RiscvCoreConfig
import CPU.TinyCore.Sim._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import lib.Sim.SpinalSim
import lib.Sim._
import lib.Sim.StramFlowSim._


class FetchTest extends AnyFunSuite{
  var compiled:SimCompiled[FetchMemory] = null
  test("compile"){
    val config = RiscvCoreConfig(addressWidth = 6,pcWidth = 6,startAddress = 0x00000000)
    compiled = SpinalSim().compile(new FetchMemory()(config))
  }

  //Todo add some assert
  test("No Branch Fetch"){
    compiled.doSim(seed = 42){
      dut =>
        dut.clockDomain.forkStimulus(10)
    }
  }



}
