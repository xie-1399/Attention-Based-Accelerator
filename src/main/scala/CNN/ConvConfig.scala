package CNN
import spinal.core._
import spinal.lib._
/**
 * datawidth
 * inputchannel
 * outputchannel
 * maxchannel
*/


object ConvType{
  //shy not use a flex filter
}

/**
 *
 * @param DATAWIDTH
 * @param OUTCHANNEL : in fact filter num = out channel num
 * @param leaky
 * @param leakyRadio
 */

case class ConvConfig(DATAWIDTH : Int,OUTCHANNEL:Int,leaky:Boolean = false,leakyRadio:Double = 0.1){

  val leakyRatio = leakyRadio
}
