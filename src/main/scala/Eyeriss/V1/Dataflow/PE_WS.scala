package Eyeriss.V1.Dataflow

import DefineSim.SpinalSim._
import spinal.core._

/* the PE with Weight stationary is a very simple dataflow */

class PE_WS(filterLen:Int,dataWidth:Int) extends PrefixComponent {
  val io = new Bundle {
    val activation = in SInt (dataWidth bits)
    val weight = in SInt (dataWidth bits)
    val pSumIn = in SInt (2 * dataWidth bits)
    val pSumOut = out SInt (2 * dataWidth bits)
  }
  io.pSumOut := io.weight * io.activation + io.pSumIn
}

class PEArray_WS(filterLen:Int,filterRowNum:Int,dataWidth:Int) extends PrefixComponent{

  val io = new Bundle{
    val Activation = in Vec(SInt(dataWidth bits),filterLen * filterRowNum)
    val Weight = in Vec(SInt(dataWidth bits),filterLen * filterRowNum)
    val pSumIn = in SInt(2 * dataWidth bits)
    val pSumOut = out SInt(2 * dataWidth bits)
  }

  val PEs = Array.fill(filterLen * filterRowNum){
    new PE_WS(filterLen, dataWidth)
  }

  /* connect the Part sum */
  PEs(0).io.pSumIn := io.pSumIn
  for(idx <- 1 until filterLen * filterRowNum){
    PEs(idx).io.pSumIn := PEs(idx - 1).io.pSumOut
  }

  /* connect the activation and weight */
  (PEs,io.Activation).zipped.map(_.io.activation := _)
  (PEs,io.Weight).zipped.map(_.io.weight := _)

  io.pSumOut := PEs(filterRowNum * filterRowNum - 1).io.pSumOut

}

object PE_WS extends App{
  val rtl = new RtlConfig().GenRTL(top = new PEArray_WS(3,3,8))
}