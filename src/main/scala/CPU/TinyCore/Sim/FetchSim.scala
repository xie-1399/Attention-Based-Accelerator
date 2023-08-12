package CPU.TinyCore.Sim

import CPU.TinyCore.RiscvCoreConfig
import CPU.TinyCore.Stages.{Fetch, InstructionBus}
import lib.Sim.SpinalSim.PrefixComponent
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

//Add Fetch Sim Component here

//just test the instruction bus(change the config address width to 6)
class InstructionBusMemory(config:RiscvCoreConfig) extends PrefixComponent{
  val io = new Bundle{
    val instructionSignal = slave (InstructionBus()(config))
  }
  val iBus = io.instructionSignal.toAxi4ReadOnly()
  val arrayBuffer = ArrayBuffer[BigInt]() //empty array buffer
  for (i <- 0 until 64) arrayBuffer += i.toBigInt

  //set some initial value for reading
  val mem = Mem(Bits(32 bits),64) initBigInt (arrayBuffer.toSeq)

  iBus.ar.ready := RegInit(True).clearWhen(iBus.ar.fire).setWhen(iBus.r.fire)
  iBus.r.valid := RegInit(False).setWhen(iBus.ar.fire).clearWhen(iBus.r.fire)
  iBus.r.payload.data := mem.readSync(iBus.ar.addr,enable = iBus.ar.fire)
}

//Fetch Memory
class FetchMemory(implicit p:RiscvCoreConfig) extends PrefixComponent{
  val fetch = new Fetch()(p)
  val memory = new InstructionBusMemory(p)
  memory.io.instructionSignal.cmd <> fetch.io.iCmd
  memory.io.instructionSignal.rsp <> fetch.io.iRsp
}

