package CNN.activation
import CNN._
import spinal.core._
//this is about the activate function leakyrelu
//f(x) = max(ax,x)

class LeakyRelu(convConfig: ConvConfig) extends Component{
  assert(convConfig.leaky,"ensure config set as the leaky")

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

    //mul when get sign equals 1
    when(overzero.sign){
      //not equals
      //out := overzero * convConfig.leakyRadio
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
