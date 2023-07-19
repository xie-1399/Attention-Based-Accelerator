package Lib.Math
import spinal.core._
import java.lang.Float._
import java.lang.Double._
//add FP32 and FP64(float and double of Scala

object Misc{
  def asBits[T](content:T): Long = {
    val value = content match {
      case Float => floatToIntBits(content.asInstanceOf[Float]) & 0x00000000ffffffffL
      case Double => doubleToLongBits(content.asInstanceOf[Double])
      case _ => 0l
    }
    value
  }


}

object Fp32{
  def expWidth = 8
  def expOffset = (1l << expWidth) - 1
  def mantWidth = 23
  def mantOffset = (1l << expWidth) - 1
  def bias = 127

  //convert Float to the bits
//  def asBits(f: Float): Long = {
//    floatToIntBits(f) & 0x00000000ffffffffL
//  }
  def asBits = Misc.asBits[Float]

  def asFloat(i:Int) = {
    intBitsToFloat(i)
  }
  def signal(f:Float) = asBits(f) >> (expWidth + mantWidth)
  def exp(f:Float) = (asBits(f) >> mantWidth) & expOffset
  def mant(f: Float) = (asBits(f)) & mantOffset

  //some value function to judge
  def isDenormal(f: Float): Boolean = {
    exp(f) == 0 && mant(f) != 0
  }

  def isZero(f: Float): Boolean = {
    exp(f) == 0 && mant(f) == 0
  }

  def isNaN(f: Float): Boolean = {
    f.isNaN()
  }

  def isInfinite(f: Float): Boolean = {
    f.isInfinite()
  }

  def isRegular(f: Float): Boolean = {
    !isInfinite(f) && !isNaN(f) && !isDenormal(f)
  }
}
