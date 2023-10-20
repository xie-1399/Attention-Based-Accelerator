package Eyeriss.V1.RLC

import DefineSim.SpinalSim.{PrefixComponent, RtlConfig}
import spinal.lib._
import spinal.core._
import spinal.lib.fsm._

import scala.math._
import DefineUntils.Counter.CounterUntil._

/* as the feature map data out , it will encode the ofMap data to the DRAM Memory
* three pair run and level are packed in the out Map
* should be more complex using the encode */

case class RLCEncoderParameters(
                                 ofMapWidth:Int = 16,
                                 encodeWidth:Int = 64,
                                 encodePairs:Int = 3,
                                 RunWidth:Int = 5,
                                 LevelWidth:Int = 16,
                                 TermWidth:Int = 1
                               ){
  require((RunWidth + LevelWidth) * encodePairs + TermWidth == encodeWidth,message = "require the 3 pairs is better ")
}

/* the output structure bundle */
case class OutMapBundle(p:RLCEncoderParameters) extends Bundle{
  val term = Bits(p.TermWidth bits)
  val runPairs = Vec(UInt(p.RunWidth bits),p.encodePairs)
  val levelPairs = Vec(UInt(p.LevelWidth bits),p.encodePairs)
}


class Encoder(p:RLCEncoderParameters,vecLen:Int = 16) extends PrefixComponent{
  import p._
  val io = new Bundle{
    /* the of map out the Relu will be UInt  */
    val ofMap = slave Stream(Vec(UInt(ofMapWidth bits),vecLen))
    val outMap = master Stream(OutMapBundle(p))
    val error = outBool()
  }

  /* test about the how much coiled zero */
  val AtProcessing = RegInit(False)
  val dataIn = RegNextWhen(io.ofMap.payload,io.ofMap.fire)

  io.ofMap.ready := !AtProcessing
  io.error := False
  io.outMap.valid := False

  val outValue = OutMapBundle(p)
  outValue.levelPairs.map(_ := 0)
  outValue.runPairs.map(_ := 0)
  outValue.term := B"1"
  val dataOut = Reg(OutMapBundle(p)).init(outValue)

  io.outMap.payload := dataOut

  val encoder = new StateMachine{
    val MapIn = new State with EntryPoint
    val Process,MapOut,Error = new State()
    val zeroCounter = Counter(pow(2,RunWidth).toInt.toBigInt)
    val runCounter = Counter(0,encodePairs)
    val levelCounter = Counter(0,encodePairs)
    val inputCounter = Counter(vecLen)
    counterInit(Seq(zeroCounter,runCounter,levelCounter,inputCounter),0)

    MapIn.whenIsActive{
      dataOut.assignFromBits(B(1,encodeWidth bits))
      zeroCounter := 0
      when(io.ofMap.fire){
        AtProcessing := True
        goto(Process)
      }
    }

    Process.whenIsActive{
      inputCounter.increment()
      when(dataIn(inputCounter) === 0){
        zeroCounter.increment()
      }.otherwise{
        dataOut.levelPairs(levelCounter) := dataIn(inputCounter)
        levelCounter.increment()
        when(zeroCounter =/= 0){
          dataOut.runPairs(runCounter) := zeroCounter
          zeroCounter := 0
          runCounter.increment()
        }
      }
      when(levelCounter.willOverflowIfInc && runCounter.willOverflowIfInc){
        goto(MapOut)
      }.elsewhen(levelCounter.willOverflow || runCounter.willOverflow){
        goto(Error)
      }
    }

    MapOut.whenIsActive{
      io.outMap.valid := True
      io.outMap.term := 0
      counterClear(Seq(zeroCounter,runCounter,levelCounter,inputCounter))
      AtProcessing := False
      goto(MapIn)
    }

    Error.whenIsActive{
      io.error := True
      counterClear(Seq(zeroCounter,runCounter,levelCounter,inputCounter))
      goto(MapIn)
    }

  }

}

object Encoder extends App{
  val rtl = new RtlConfig().GenRTL(top = new Encoder(RLCEncoderParameters(),11))
}