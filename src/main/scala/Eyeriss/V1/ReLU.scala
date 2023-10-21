package Eyeriss.V1

import DefineSim.SpinalSim.{PrefixComponent, RtlConfig}
import spinal.core._
import spinal.lib._

/* the relu function y = z(z > 0 ) and y = 0 (z < 0)
* and using the relu function can change the SInt to the UInt */

case class ActParameters(
                        dataWidth:Int = 16,
                        dataLen:Int = 32,
                        pipe:Boolean = true /*  build the pipeline */
                        ){
  def bitsNum  = dataLen * dataWidth
}

class ReLU(p:ActParameters) extends PrefixComponent{
  import p._

  val zero = S(0,dataWidth bits)

  val io = new Bundle{
    val dataIn = slave Stream(Vec(SInt(dataWidth bits),dataLen))
    val dataOut = master Stream(Vec(SInt(dataWidth bits),dataLen))
  }
  val busy = RegInit(False)
  io.dataIn.ready := !busy

  /* one cycle get value */
  val logic = ifGen(!pipe){
    busy := False
    val sequence = io.dataIn.payload.map(x => Mux(x > 0, x , zero))
    io.dataOut.payload.assignFromBits(sequence.asBits())
    io.dataOut.valid := io.dataIn.valid
  }

  val pipeline = ifGen(pipe){} // Todo

}

object ReLU extends App{
  val rtl = new RtlConfig().GenRTL(top = new ReLU(ActParameters(pipe = false)))
}