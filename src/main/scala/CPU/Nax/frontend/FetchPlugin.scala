package CPU.Nax.frontend
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._
import CPU.Nax.utils._
import CPU.Nax.Interface._


/*
Fetch instruction from I-Cache
How to create pipeline using the spinal
*/

trait FetchPipelineRequirements{
  def stagesCountMin:Int
}

//or just extends Area
object FetchPlugin extends AreaRoot {
  //using stageable value in pipeline
  val FETCH_ID = Stageable(UInt(12 bits))
}

class FetchPlugin extends Plugin with LockedImpl{

  //get the max value of FetchPipelineRequirements object
  val pipeline = create early new Pipeline{
    //create the pipeline using the lib function
    val stagesCount = framework.getServices.map{ //list.map
      case s:FetchPipelineRequirements => s.stagesCountMin
      case _ => 0
    }.max
    //create three stages (as the frontend)
    val stages = Array.fill(stagesCount)(newStage())

    //connect the stage
    import spinal.lib.pipeline.Connection._
    for((m,s) <- (stages.dropRight(1),stages.tail).zipped){ //dropRight remove last value(use zipped to produce one)
        connect(m,s)(M2S(flushPreserveInput = m == stages.head))
    }
  }
  pipeline.setCompositeName(this)

  val fetchId = create late new Area {
    
    val stage = pipeline.stages(1)
  }

  //some methods to get the frontend stages and pipeline
  def getStage(id:Int) = pipeline.stages(id)
  def getLastStage = pipeline.stages.last
  def getPipeline() = pipeline.get
}
