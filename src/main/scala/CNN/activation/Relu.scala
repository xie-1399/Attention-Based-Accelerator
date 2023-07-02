package CNN.activation

import CNN.ConvConfig
import spinal.core._

//a little simple
class Relu(convConfig: ConvConfig) extends Component{

  val io = new Bundle{
    val inputData = in Vec(SInt(convConfig.DATAWIDTH bits),
      convConfig.OUTCHANNEL)

    val outputData = out Vec(SInt(convConfig.DATAWIDTH bits),
      convConfig.OUTCHANNEL)

  }
  noIoPrefix()
  val leaky = U((convConfig.leakyRadio * scala.math.pow(2,17)).toInt,16 bits)
  val mid = U((0.5 * scala.math.pow(2,17)).toInt,17 bits)

  def leakydata(overzero: SInt, bool: Boolean): SInt = {
    val out = SInt(convConfig.DATAWIDTH bits)

    when(overzero.sign){
      out := 0
    }.otherwise{
      out := overzero
    }


    out
  }

  (0 until convConfig.OUTCHANNEL) .foreach(
    i =>
    {
      io.outputData(i) := leakydata(io.inputData(i), i == 0)
    }
  )
}


object Relu extends App{
  SpinalVerilog(new Relu(ConvConfig(8,3)))
}