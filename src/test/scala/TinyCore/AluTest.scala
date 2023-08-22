package TinyCore
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import lib.Sim.SpinalSim
import CPU.TinyCore._
import CPU.TinyCore.DecodeInfo._
import scala.util.Random


class AluTest extends AnyFunSuite{
  var compiled:SimCompiled[Alu] = null
  test("compile"){
    compiled =SpinalSim().compile(new Alu)
  }

  test("testbench"){
    compiled.doSim(seed = 42){
      dut =>
        dut.clockDomain.forkStimulus(10)

        def bench(dut:Alu,iter:Int = 20): Unit = {
          for(idx <- 0 until iter){
            val func = List(ALU.ADD,ALU.SLTU,ALU.SLT,ALU.SUB,ALU.OR,ALU.XOR,ALU.AND,ALU.COPY)
            val randFunc = Random.nextInt(8)
            dut.io.func #= func(randFunc)
            val src0 = Random.nextInt(4096 * 16)
            val src1 = Random.nextInt(4096 * 16)
            dut.io.src0 #= src0
            dut.io.src1 #= src1
            dut.io.doSub #= randFunc == 3

            val value =  if (src0 < src1) 1 else 0
            dut.clockDomain.waitSampling()
            randFunc match {
              case 0 => assert(dut.io.result.toBigInt == src1 + src0,"ADD wrong")
              case 1 => assert(dut.io.result.toBigInt == value,"SLTU wrong")
              case 2 => assert(dut.io.result.toBigInt == value," SLT wrong")
              case 3 => assert(dut.io.actual.toBigInt == src0 - src1,"SUB wrong")
              case 4 => assert(dut.io.result.toBigInt == (src1 | src0),"OR wrong")
              case 5 => assert(dut.io.result.toBigInt == (src1 ^ src0),"XOR wrong")
              case 6 => assert(dut.io.result.toBigInt == (src1 & src0),"AND wrong")
              case 7 => assert(dut.io.result.toBigInt == src0,"COPY wrong")
              case _ =>
            }
          }
        }
        bench(dut,5000)
    }
  }
}
