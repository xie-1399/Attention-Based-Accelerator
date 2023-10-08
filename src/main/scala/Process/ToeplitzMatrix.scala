package Process

import DefineSim.SpinalSim.PrefixComponent
import spinal.core._
import spinal.core.internals.Operator
import spinal.lib._

 /*
 * the hardware of ToeplitzMatrix calculation
 * should think about the size mapping
 * Todo about the calculate
 */

case class filterSize(row:Int,
                     col:Int,
                     dataBits:Int,
                     channels:Int, stride:Int)

case class featureMapSize(row: Int,
                          col: Int,
                          dataBits: Int,
                          channels: Int)

case class calculateConfig(fillFilter:Boolean = true,fillFeatureMap:Boolean = false){
  if(fillFeatureMap && fillFilter) assert(false,"only support fill featureMap or Filter for one")
}


class ToeplitzMatrix(filtersize:filterSize,featuremapsize:featureMapSize,c:calculateConfig) extends PrefixComponent{
  assert(filtersize.channels == featuremapsize.channels)
  val filterDepth = filtersize.row * filtersize.col * filtersize.channels
  val featureMapDepth = featuremapsize.row * featuremapsize.col * featuremapsize.channels

  val io = new Bundle{
    val filter = in Vec(UInt(filtersize.dataBits bits),filterDepth)
    val featuremap = in Vec(UInt(featuremapsize.dataBits bits),featureMapDepth)

    val result = out(UInt(Math.max(filtersize.dataBits,featuremapsize.dataBits) bits))  //cycle gets value
  }

  val calculate = if(c.fillFilter){
    new Area {
      /* fill the filter with zero */



    }
  }
  else{
    new Area{

    }
  }
}

 /*
 first should understand how to write the mapping
 */

class paddingZero(filtersize:filterSize,featuremapsize:featureMapSize) extends PrefixComponent{

  val rowOut = (featuremapsize.row - filtersize.row) / filtersize.stride + 1
  val colOut = (featuremapsize.col - filtersize.col) / filtersize.stride + 1
  val numberOut = rowOut * colOut
  val fillingsize = featuremapsize.row * featuremapsize.col

  val io = new Bundle{
    val value = in Vec(UInt(filtersize.dataBits bits),filtersize.row * filtersize.col)
  }
  val inner = Vec(UInt(filtersize.dataBits bits),numberOut)
  inner.map(_:=0)



}