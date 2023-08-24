package TinyCore

//test the decode unit

import CPU.TinyCore.RiscvCoreConfig
import CPU.TinyCore.Stages._
import CPU.TinyCore.Sim._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import lib.Sim._


class DecodeTest extends AnyFunSuite{
  var compiled:SimCompiled[instructionctrl] = null
  test("compile"){
    compiled = SpinalSim().compile{
      val dut = new instructionctrl
      dut.ctrl.simPublic()
      dut
    }
  }

  //a online page:https://hggshiwo.github.io/rimulator/index.html
  test("instructionCtrl Test"){
    compiled.doSim(seed = 42) {
      dut =>
        //Todo add more instruction
        dut.clockDomain.forkStimulus(10)
        def generateCode(opretion:String): BigInt = {
          val code = opretion match {
            case "ADD" => 0x003080b3
            case "SUB" => 0x403100b3
            case "SW" => 0x00152223
            case "LD" => 0x00452083
            case "ADDI" => 0x00408093
            case "AUIPC" => 0x3adeaa17
            case "LUI" => 0x004649b7
            case "JALR" => 0x064404e7
            case _ => 0
          }
          code
        }
        //just a simple test it
        val codeName = List[String]("ADD","SUB","SW","LD","ADDI","AUIPC","LUI","JALR")
        for (idx <- 0 until codeName.length){
          val code = generateCode(codeName(idx))
          dut.io.instruction #= code
          dut.clockDomain.waitSampling()
        }
    }
  }
}

// another way to use the sim situation

class DecodeSimTest extends AnyFunSuite{
  test("only sim the decode"){
    SIMCFG().compile{
      val config = RiscvCoreConfig()
      val dut = new Decode()(config)
      dut
    }.doSim{
      dut =>
    }
  }

  test("sim the decode with fetch") {
    SIMCFG(gtkFirst = true).compile {
      val config = RiscvCoreConfig(addressWidth = 6,pcWidth = 6,startAddress = 0x00000000)
      val dut = new DecodeSim()(config)
      dut
    }.doSim {
      dut =>
    }
  }


}