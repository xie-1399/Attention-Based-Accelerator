package Eyeriss.V1

import DefineSim.SpinalSim.PrefixComponent
import spinal.lib._
import spinal.core._


/* the PE module in the eyeriss to support the RS dataflow*/

case class EyerissParameters(
                            filterNumWidth:Int = 8,
                            filterLenWidth:Int = 8,
                            imgNumWidth:Int = 8,
                            imgLenWidth:Int = 8,
                            channelNumWidth:Int = 8,
                            )

case class PEConfig(p:EyerissParameters) extends Bundle{
  import p._
  val filterNum = UInt(filterNumWidth bits)
  val imgNum = UInt(imgNumWidth bits)
  val singlefilterLen = UInt(filterLenWidth bits)
  val singleimgLen = UInt(imgLenWidth bits)
  val channel = UInt(channelNumWidth bits)
  val relu = Bool()
}

class EyerissPE(p:EyerissParameters,filterSpadLen:Int = 225,psumMemLen:Int = 225,fmapSpadLen:Int = 225,dataWidth:Int = 16,position:(Int,Int) = (0,0)) extends PrefixComponent{

  val io = new Bundle{
    /* PE State control*/
    val state = in UInt(2 bits)
    val config = in (PEConfig(p))
    val filter = slave Stream (SInt(dataWidth bits))
    val img = slave Stream (SInt(dataWidth bits))
    val pSumIn = slave Stream (SInt(2 * dataWidth bits))
    val totalFilterNum = in(UInt(16 bits))

    val stateOut = out UInt(4 bits)
    val done = out Bool()
    val pSumSRAM = master Stream(SInt(2 * dataWidth bits))
  }

  val stateMachine =

}
