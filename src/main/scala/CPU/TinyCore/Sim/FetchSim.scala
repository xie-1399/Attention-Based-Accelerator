package CPU.TinyCore.Sim

import CPU.TinyCore.RiscvCoreConfig
import CPU.TinyCore.Stages.{Fetch, InstructionBus}
import lib.Sim.SpinalSim.PrefixComponent
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

//Add Fetch Sim Component here

//just test the instruction bus(change the config address width to 6)
class InstructionBusMemory(config:RiscvCoreConfig,fetchNum:Boolean = false) extends PrefixComponent{
  val io = new Bundle{
    val instructionSignal = slave (InstructionBus()(config))
  }
  val iBus = io.instructionSignal.toAxi4ReadOnly()
  val arrayBuffer = ArrayBuffer[BigInt]() //empty array buffer
  if(fetchNum){
    for (i <- 0 until 64) arrayBuffer += i.toBigInt
  }
  else {
    //just test some simple instructions
    arrayBuffer += (
      0xed428293l, //add
      0x4c771663l, //bne
      0xffff8137l, //lui
      0x01f51513l  //sll
    )
    for (i <- 0 until 60) arrayBuffer += 0x00000013
  }

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

