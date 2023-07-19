package CPU.Nax.fetch

import CPU.Nax.Interface._
import CPU.Nax.utils._
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._
//all stages in the frontend including dispatch

class FrontendPlugin() extends Plugin with LockedImpl {
  val pipeline = create early new Pipeline {
    val stagesCount = framework.getServices.map {
      case s: FetchPipelineRequirements => s.stagesCountMin
      case _ => 0
    }.max
    val aligned = newStage()
    val decompressed = newStage()
    val decoded = newStage()
    val serialized = newStage()
    val allocated = newStage()
    val dispatch = newStage()

    import spinal.lib.pipeline.Connection._
    //what happened about DIRECT and M2S
    connect(serialized,allocated)(DIRECT())
    connect(allocated,dispatch)(M2S())

    val isBusy = Bool()
    val isBusyAfterDecode = Bool()
  }

  pipeline.setCompositeName(this)

  val builder = create late new Area {
    //Todo how the commit unit effect the frontend

    lock.await()
    //stageSet: order and changeble set
    pipeline.isBusy := (pipeline.stagesSet - pipeline.aligned).map(_.isValid).toList.orR  //check the frontend is busy
    pipeline.isBusyAfterDecode := List(pipeline.serialized, pipeline.allocated, pipeline.dispatch).map(_.isValid).toList.orR
    //build the Pipeline
    pipeline.build()
  }
}
