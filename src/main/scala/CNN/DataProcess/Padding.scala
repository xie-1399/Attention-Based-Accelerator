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
  val left = 1
  val right = 2
  val up = 3
  val down = 4
}

//Todo just padding one pic each
case class PaddingConfig(datawidth:Int,channelwidth:Int,computenum:Int,rowwidth:Int,colwidth:Int,paddingsize:Int){
  val PictureNum = 1
  val StreamDatawidth = datawidth * PictureNum * computenum //each Stream carry data width
}

class Padding(paddingType: String,paddingConfig: PaddingConfig) extends Component{
  val padType: String = paddingType
  require(padType == PaddingType.padDefault || padType == PaddingType.noPad,"Only support valid and the same padding")
  val io = new Bundle{
    val inputData = slave Stream(UInt(paddingConfig.StreamDatawidth bits))
    val outputData = master Stream(UInt(paddingConfig.StreamDatawidth bits))

    // 0->left 1->rigth 2->up 3->down
    val enPadding = in Vec(Bool(),4) //direction
    val start = in Bool()

    val inChannel = in UInt(paddingConfig.channelwidth bits)
    val inRownum = in UInt(paddingConfig.rowwidth bits)
    val outRownum = out UInt(paddingConfig.rowwidth + 1 bits) //padding row
    val inColnum = in UInt(paddingConfig.colwidth bits)
    val outColnum = out UInt(paddingConfig.colwidth + 1 bits)  //padding col
    //use shich value to padding
    val padvalue = in UInt(paddingConfig.datawidth bits)

  }
  noIoPrefix()
  val maxRownum = 2 ^ (paddingConfig.rowwidth) - 1
  val maxColnum = 2 ^ (paddingConfig.colwidth) - 1
  val maxoutRownum = 2 ^ (paddingConfig.rowwidth + 1) - 1
  val maxoutColnum = 2 ^ (paddingConfig.colwidth + 1) - 1
  require(2 * paddingConfig.paddingsize + maxRownum <= maxoutRownum,"row padding size is too large")
  require(2 * paddingConfig.paddingsize + maxColnum <= maxoutColnum,"col padding size is too large")

  val leftRight = io.enPadding(0) ## io.enPadding(1)
  val upDown = io.enPadding(2) ## io.enPadding(3)

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
    val rowcounter = RegNext(io.inRownum)
    val colcounter = RegNext(io.inColnum)
    val channelcounter = RegNext(io.inChannel)

    //Todo
    val fsm = new StateMachine{
      val IDLE = new State() with EntryPoint
      val BUILD = new State()
      val LEFT = new State()
      val RIGHT = new State()
      val UP = new State()
      val Down = new State()
      val LAST = new State()

      //hold 16 streams
      val fifo = StreamFifo(cloneOf(io.inputData.payload),16)
      val streamzero = Stream(io.padvalue)
      //padding order: up -> left -> right -> down
      val fillUp = RegInit(False)
      val fillDown = RegInit(False)
      val fillLeft = RegInit(False)
      val fillRight = RegInit(False)
      IDLE.whenIsActive{
        when(io.start){
          goto(BUILD)
        }
      }

      BUILD.whenIsActive{
          when(io.enPadding(2) && !fillUp){
            goto(UP)
          }.elsewhen(io.enPadding(0) && !fillLeft){
            goto(LEFT)
          }.elsewhen(io.enPadding(1) && !fillRight){
            goto(RIGHT)
          }.otherwise{
            goto(Down)
          }
      }

      UP.whenIsActive{

      }

      LEFT.whenIsActive{

      }

      RIGHT.whenIsActive{

      }

      Down.whenIsActive{

      }

      LAST.whenIsActive{

      }


    }
  }
  val noPadding = (padType == PaddingType.noPad) generate new Area {
    //just in
    io.outputData << io.inputData
  }
}

object Padding extends App{
  val paddingConfig = PaddingConfig(8, 2, 8, 4, 4, 2)
  SpinalVerilog(new Padding("valid",paddingConfig))
}