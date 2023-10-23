package Eyeriss.V1

import DefineMem.FIFO
import DefineSim.SpinalSim.{PrefixComponent, RtlConfig}
import spinal.core._
import spinal.lib._

/* the relu function y = z(z > 0 ) and y = 0 (z < 0)
* and using the relu function can change the SInt to the UInt */

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


  /* seem the fifo structure is not ready */

  val pipeline = ifGen(pipe){
    /* build it with pipeline */

    val fifo = new FIFO(io.dataIn.payloadType,entries = fifoDepth) /* the fifo is used to save the payload */
    io.dataIn >-> fifo.io.enqueue

    fifo.io.dequeue.ready := True
    val pass = False
    io.dataOut.valid := False

    val stage1 = new Area {
      val load = fifo.io.dequeue.payload
      val sequence = load.map(x => Mux(x > 0, x , zero))
      when(fifo.io.dequeue.fire){
        pass := True
      }
    }

    val stage2 = new Area{
      when(pass){
        fifo.io.dequeue.ready := False
        io.dataOut.valid := True
      }
      io.dataOut.payload.assignFromBits(stage1.sequence.asBits())

    }
    io.block := fifo.io.count === fifoDepth - 1

  }

}

object ReLU extends App{
  val rtl = new RtlConfig().GenRTL(top = new ReLU(ActParameters(pipe = true)))
}