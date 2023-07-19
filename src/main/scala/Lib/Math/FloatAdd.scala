package Lib.Math
import spinal.core._
import spinal.lib._
// float add arithmetic

case class FloatAddConfig(pipeStages:Int = 1){}

class FloatAdd(floatAddConfig: FloatAddConfig = null) extends Component {
  def pipeStages = if(floatAddConfig == null) 1 else floatAddConfig.pipeStages //set the pipeline stage

  val io = new Bundle{
    val
  }
}
