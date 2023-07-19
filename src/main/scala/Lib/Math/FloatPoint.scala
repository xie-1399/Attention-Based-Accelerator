package Lib.Math

import spinal.core._

/*
this is base Bundle of the float point design
IEEE -< V = (-1)sign × M × pow(2,E)
some examples: https://www.toolhelper.cn/Digit/FractionConvert
*/

case class FPConfig(expSize:Int,mantSize:Int){
  //base formal of float
  def fullSize = 1 + expSize + mantSize
  def bias = (1 << (expSize - 1)) - 1
}

object FloatPoint{
  //use object get a FP
  def apply(expSize:Int,mantSize:Int):FloatPoint = {
    new FloatPoint(FPConfig(expSize = expSize,mantSize = mantSize))
  }

  def apply(fpConfig: FPConfig): FloatPoint = {
    new FloatPoint(fpConfig)
  }
}


class FloatPoint(fpConfig: FPConfig) extends Bundle{
  val signal = Bool()  //signal
  val exp = UInt(fpConfig.expSize bits)
  val mant = UInt(fpConfig.mantSize bits)

  //init with some value
  def init() = {
    signal init(False)
    exp init(0)
    mant init(0)
    this
  }

  //whether is normal number or special number
  def is_zero():Bool = exp === 0
  def is_Nan():Bool = exp.andR && mant.orR //not a number
  def is_inf():Bool = exp.andR && !mant.orR

  def set_Zero() = {
    signal := False
    exp := 0
    mant := 0
  }

  //get the real M (1 + f)
  def fullmant(): UInt = {
    (mant.resized(fpConfig.mantSize + 1).asBits | (U(1) << fpConfig.mantSize).asBits).asUInt
  }

  //get abs value
  def absolute():FloatPoint = {
    val abs = FloatPoint(fpConfig)
    abs.signal := False
    abs.exp := exp
    abs.mant := mant
    abs
  }
  //convert to Vec
  def toVec():Bits = {
    signal ## exp.asBits ## mant.asBits
  }
  // Vec convert to FP
  def fromVec(vec:Bits) = {
    signal := vec(fpConfig.expSize + fpConfig.mantSize)
    exp := vec(fpConfig.mantSize,fpConfig.expSize bits).asUInt  //offset , width
    mant := vec(0,fpConfig.mantSize bits).asUInt
  }


  //Todo convert double to FP
}
