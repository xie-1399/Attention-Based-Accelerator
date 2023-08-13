package CPU.TinyCore
import CPU.TinyCore.Stages._
import spinal.core._
import spinal.lib._
import Extension._

import scala.collection.mutable.ArrayBuffer

//set the core config and finish the pipeline

case class RiscvCoreConfig(val pcWidth:Int = 32,
                           val addressWidth : Int = 32,
                           val startAddress : Long = 0x80000000l,
                           val bypassExcute0 : Boolean = true,
                           val bypassExcute1 : Boolean = true,
                           val bypassWriteBack : Boolean = true,
                           val bypassWriteBackBuffer : Boolean = true,
                           val fastFetchCmdPcCalculation : Boolean = false,
                           val branchPrediction: BranchPrediction = static,   //set branch prediction type
                           val regfileReadKind : RegfileReadKind = async
                          //Todo add more configs
                          ){
  val extensions = ArrayBuffer[CoreExtension]()
  //add Extension here
  def add[T<:CoreExtension](that : T) : T = {
    extensions += that
    that
  }
  def needExcute0PcPlus4 = true
}


class Core {

}
