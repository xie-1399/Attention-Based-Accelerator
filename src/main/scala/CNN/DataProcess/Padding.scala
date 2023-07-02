package CNN.DataProcess

import spinal.core.Component.push
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

//need to padding thr pic(valid padding or same padding)
object PaddingType{
  val padDefault = "same"
  val noPad = "valid"
}

object PaddingDirection{
  val leftIndex = 0
  val rightIndex = 1
  val upIndex = 2
  val downIndex = 3
}

//Todo just padding one pic each and padding should hold left and right at the same time
case class PaddingConfig(datawidth:Int,channelwidth:Int,computenum:Int,rowwidth:Int,colwidth:Int,paddingsize:Int,nopadding:Boolean = false){
  val PictureNum = 1
  //each stream deal with one line
  val StreamOutDatawidth = if (nopadding) datawidth * PictureNum * computenum
                        else datawidth * PictureNum * (computenum + paddingsize * 2) //each Stream carry data width
  val StreamInDatawidth = datawidth * PictureNum * computenum
}

class Padding(paddingType: String,paddingConfig: PaddingConfig) extends Component{
  val padType: String = paddingType
  require(padType == PaddingType.padDefault || padType == PaddingType.noPad,"Only support valid and the same padding")
  val validArea = padType == "valid"
  val sameArea = padType == "same"
  require(validArea == paddingConfig.nopadding,"no padding should use the valid Area")


  val io = new Bundle{
    val inputData = slave Stream(UInt(paddingConfig.StreamInDatawidth bits))
    val outputData = master Stream(UInt(paddingConfig.StreamOutDatawidth bits))

    // 0->left 1->rigth 2->up 3->down
    val enPadding = in Vec(Bool(),4) //direction
    val start = in Bool()

    val inChannel = in UInt(paddingConfig.channelwidth bits)
    val inChannelnum = in UInt(paddingConfig.channelwidth bits)
    val inRownum = in UInt(paddingConfig.rowwidth bits)
    val outRownum = out UInt(paddingConfig.rowwidth + 1 bits) //padding row
    val inColnum = in UInt(paddingConfig.colwidth bits)
    val outColnum = out UInt(paddingConfig.colwidth + 1 bits)  //padding col
    //use shich value to padding
    val padvalue = in Bits(paddingConfig.datawidth bits)
    val finish = out Bool()
  }
  noIoPrefix()

  val maxRownum = 2 ^ (paddingConfig.rowwidth) - 1
  val maxColnum = 2 ^ (paddingConfig.colwidth) - 1
  val maxoutRownum = 2 ^ (paddingConfig.rowwidth + 1) - 1
  val maxoutColnum = 2 ^ (paddingConfig.colwidth + 1) - 1

  require(2 * paddingConfig.paddingsize + maxRownum <= maxoutRownum,"row padding size is too large")
  require(2 * paddingConfig.paddingsize + maxColnum <= maxoutColnum,"col padding size is too large")

  val leftRight = io.enPadding(PaddingDirection.leftIndex) ## io.enPadding(PaddingDirection.rightIndex)
  val upDown = io.enPadding(PaddingDirection.upIndex) ## io.enPadding(PaddingDirection.downIndex)

  //get out col and row num(1111-> row and col each +2)
  def getRowColnum(direction:Bits,num:UInt,paddingsize:Int) = {
    val out = Reg(UInt(num.getWidth + 1 bits))
    switch(direction){
      is(0){
        out := num.resized
      }
      //like this equal some cases
      is(1,2){
        out := (num +^ (paddingsize) ).resized //with carry
      }
      is(3){
        out := (num +^ (2 * paddingsize)).resized
      }
    }
    out
  }

  io.outRownum := getRowColnum(leftRight,io.inRownum,paddingConfig.paddingsize)
  io.outColnum := getRowColnum(upDown,io.inColnum,paddingConfig.paddingsize)

  //padding the input data with 0
  val defaultPadding = (padType == PaddingType.padDefault) generate new Area {
    val rownum = RegNext(io.inRownum)
    val colnum = RegNext(io.inColnum)
    val channelnum = RegNext(io.inChannel)
    val paddingSizeCounter = Counter(paddingConfig.paddingsize)
    val centerCounter = Counter(maxoutRownum)
    //val otherChannel = RegInit(False).setWhen(io.start).clearWhen(fsm.fillDown)

    //hold 16 streams
    val fifo = StreamFifo(cloneOf(io.outputData.payload),16)

    val fsm = new StateMachine{
      val IDLE = new State() with EntryPoint
      val BUILD = new State()
      val CENTER = new State()
      val UP = new State()
      val Down = new State()

      //padding order: up -> (left + right) -> down
      val fillUp,fillDown,fillCenter  = RegInit(False)
      val continueStream = RegInit(False)
      val centerData = Stream(cloneOf(io.outputData.payload))
      val zeroData = Stream(cloneOf(io.outputData.payload))

      IDLE.whenIsActive{
        continueStream := False
        fillUp := False
        fillDown := False
        fillCenter := False
        when(io.start){
          goto(BUILD)
        }
      }

      BUILD.whenIsActive{
        continueStream := False
        //like 0011 fill up and down
        when(io.enPadding(PaddingDirection.upIndex) && !fillUp){
            goto(UP)
        }.elsewhen(io.enPadding(PaddingDirection.leftIndex )&& io.enPadding(PaddingDirection.rightIndex) && !fillCenter){
            goto(CENTER)
        }.elsewhen(io.enPadding(PaddingDirection.downIndex) && !fillDown) {
            goto(Down)
        }.otherwise{
          //Todo check if is start
          goto(IDLE)
        }
      }

      UP.whenIsActive{
        continueStream := False
        //need to fill one padding size
        zeroData.valid := True
        zeroData.payload := 0
        zeroData >> fifo.io.push
        paddingSizeCounter.increment()
        when(paddingSizeCounter.willOverflowIfInc){
          paddingSizeCounter.clear()
          fillUp := True
          goto(CENTER)
        }
      }

      CENTER.whenIsActive{
        continueStream := True
        when(io.inputData.fire){
          //Todo
          centerData.valid := io.inputData.valid
          centerData.payload := (io.padvalue ## io.padvalue ## io.inputData.asBits.resize(paddingConfig.StreamInDatawidth) ## io.padvalue ## io.padvalue).asUInt
          centerData >> fifo.io.push
          centerCounter.increment()
        }
        when(centerCounter.value === io.inRownum){
             goto(Down)
        }
      }

      Down.whenIsActive{
        continueStream := False
        zeroData.valid := True
        zeroData.payload := 0
        zeroData >> fifo.io.push
        paddingSizeCounter.increment()
        when(paddingSizeCounter.willOverflowIfInc) {
          paddingSizeCounter.clear()
          fillDown := True
          goto(IDLE)
        }
      }

    }
    io.inputData.continueWhen(fsm.continueStream)
    io.outputData << fifo.io.pop
    io.finish := False
    when(io.inChannel === io.inChannelnum) {
      io.finish := True
    }
    when(io.finish === True) {
      io.outputData.payload := 0
      io.outputData.valid := False
    }

  }

  val noPadding = (padType == PaddingType.noPad) generate new Area {
    //without padding
    io.finish := False
    io.outputData << io.inputData
    when(io.inChannel === io.inChannelnum){
      io.finish := True
    }
    when(io.finish === True){
      io.outputData.payload := 0
      io.outputData.valid := False
    }
  }

}

object Padding extends App{
  val paddingConfig = PaddingConfig(8, 2, 8, 4, 4, 2)
  SpinalVerilog(new Padding("same",paddingConfig))
}