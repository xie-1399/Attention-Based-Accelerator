package CPU.TinyCore.Sim
import lib.Sim.SpinalSim.PrefixComponent
import spinal.core._
import spinal.lib._
import CPU.TinyCore.DecodeInfo._
//some component about the riscv decode

class instructionctrl extends PrefixComponent{
  val io = new Bundle{
    val instruction = in Bits(32 bits)
  }
  //check the instruction information
  val ctrl = InstructionCtrl(io.instruction)
}


class DecodeSim {

}
