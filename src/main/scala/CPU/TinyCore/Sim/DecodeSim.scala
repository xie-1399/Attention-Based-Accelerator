package CPU.TinyCore.Sim
import lib.Sim.SpinalSim.PrefixComponent
import spinal.core._
import spinal.lib._
import CPU.TinyCore.Stages._
import CPU.TinyCore.DecodeInfo._
import CPU.TinyCore.RiscvCoreConfig
//some component about the riscv decode

class instructionctrl extends PrefixComponent{
  val io = new Bundle{
    val instruction = in Bits(32 bits)
  }
  //check the instruction information
  val ctrl = InstructionCtrl(io.instruction)
}


class DecodeSim(implicit p:RiscvCoreConfig) extends PrefixComponent {
  val fetch = new Fetch()(p)
  val decode = new Decode()(p)
  val regfile = new Regfile()   //async
  val memory = new InstructionBusMemory(p)

  memory.io.instructionSignal.cmd <> fetch.io.iCmd
  memory.io.instructionSignal.rsp <> fetch.io.iRsp

  //connect teh regfile IO
  fetch.Fetch.outInst >-> decode.io.inInst
  fetch.io.pcLoad <> decode.io.pcLoad
  regfile.io <> decode.io.regfileIO

}
