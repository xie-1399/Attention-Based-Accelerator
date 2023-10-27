package Eyeriss.V1

import DefineMem.FIFO
import DefineSim.SpinalSim.{PrefixComponent, RtlConfig}
import spinal.core._
import spinal.lib._

/* the relu function y = z(z > 0 ) and y = 0 (z < 0)
* and using the relu function can change the SInt to the UInt
* using fifo to store data first and then deal with it */

case class ActParameters(
                        dataWidth:Int = 16,
                        dataLen:Int = 32,
                        pipe:Boolean = true, /*  build the pipeline */
                        fifoDepth:Int = 16
                        ){
  def bitsNum  = dataLen * dataWidth
}

class ReLU(p:ActParameters) extends PrefixComponent{
  import p._

  val zero = S(0,dataWidth bits)

  val io = new Bundle{
    val dataIn = slave Stream(Vec(SInt(dataWidth bits),dataLen))
    val dataOut = master Stream(Vec(SInt(dataWidth bits),dataLen))
    val block = ifGen(pipe) { out Bool() }
  }

  /* one cycle get value */
  val logic = ifGen(!pipe){
    val busy = RegInit(False)
    io.dataIn.ready := !busy
    busy := False
    val sequence = io.dataIn.payload.map(x => Mux(x > 0, x , zero))
    io.dataOut.payload.assignFromBits(sequence.asBits())
    io.dataOut.valid := io.dataIn.valid
  }


  val pipeline = ifGen(pipe) {
    /* build it with pipeline */

    val fifo = new FIFO(io.dataIn.payloadType,entries = fifoDepth) /* the fifo is used to save the payload */
    fifo.io.enqueue.connectFrom(io.dataIn)
    val process = RegInit(True)
    val pass = RegInit(False)

    fifo.io.dequeue.ready := process
    io.dataOut.valid := False

    val stage1 = new Area {
      val sequence = RegNextWhen(fifo.io.dequeue.payload.map(x => Mux(x > 0, x , zero)).asBits(),fifo.io.dequeue.fire).init(0)
      when(fifo.io.dequeue.fire){
        process := False
        pass := True
      }
    }

    val stage2 = new Area{
      when(pass){
        io.dataOut.valid := True
        pass := False
        process := True
      }
    }
    io.block := fifo.io.count === fifoDepth - 1
    io.dataOut.payload.assignFromBits(stage1.sequence)
  }

}

object ReLU extends App{
  val rtl = new RtlConfig().GenRTL(top = new ReLU(ActParameters(pipe = true)))
}