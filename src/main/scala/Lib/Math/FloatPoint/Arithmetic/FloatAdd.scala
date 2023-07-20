package Lib.Math.FloatPoint.Arithmetic

import Lib.Math.FloatPoint._
import spinal.core._

//the add step : https://juejin.cn/post/7085316206239940639

case class FpAddConfig(pipeStages: Int = 1){} //set initial pipeline stages

class FloatAdd(fpConfig:FPConfig,addConfig: FpAddConfig = null) extends Component{
  def pipeStages = if (addConfig == null) 1 else addConfig.pipeStages
  val io = new Bundle{
    val opValid = in Bool()
    val opRs1 = in (FPBase(fpConfig)) //set in with ()
    val opRs2 = in (FPBase(fpConfig))
    //wait for the results
    val resultValid = out Bool()
    val resultValue = out (FPBase(fpConfig))
  }

  //stage0 (check some special values)
  val stage0_Valid = io.opValid
  val stage0_Rs1 = io.opRs1
  val stage0_Rs2 = io.opRs2

  val stage0_Rs1_isZero = io.opRs1.is_zero()
  val stage0_Rs2_isZero = io.opRs2.is_zero()

  val stage0_Rs1_isInf = io.opRs1.is_inf()
  val stage0_Rs2_isInf = io.opRs2.is_inf()

  //stage1

}
