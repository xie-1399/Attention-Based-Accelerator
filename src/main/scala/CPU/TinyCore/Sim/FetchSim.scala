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
      0xed,0x42,0x82,0x93, //add
      0x4c,0x77,0x16,0x63, //bne
      0xff,0xff,0x81,0x37, //lui
      0x01,0xf5,0x15,0x13  //sll
    )
    for (i <- 0 until 12) arrayBuffer += (0x00l,0x00l,0x00l,0x13l)  //nop
  }

  //set some initial value for reading
  val mem = Mem(Bits(8 bits),64) initBigInt (arrayBuffer.toSeq)

  iBus.ar.ready := RegInit(True).clearWhen(iBus.ar.fire).setWhen(iBus.r.fire)
  iBus.r.valid := RegInit(False).setWhen(iBus.ar.fire).clearWhen(iBus.r.fire)

  iBus.r.payload.data := mem.readSync(iBus.ar.addr,enable = iBus.ar.fire) ## mem.readSync(iBus.ar.addr + 1,enable = iBus.ar.fire) ##
    mem.readSync(iBus.ar.addr + 2,enable = iBus.ar.fire) ## mem.readSync(iBus.ar.addr + 3,enable = iBus.ar.fire)
}

//Fetch Memory
class FetchMemory(implicit p:RiscvCoreConfig) extends PrefixComponent{
  val fetch = new Fetch()(p)
  val memory = new InstructionBusMemory(p)
  memory.io.instructionSignal.cmd <> fetch.io.iCmd
  memory.io.instructionSignal.rsp <> fetch.io.iRsp
}


