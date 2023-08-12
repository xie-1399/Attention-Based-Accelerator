package CPU.TinyCore.Stages

import CPU.TinyCore.RiscvCoreConfig
import spinal.core._
import spinal.lib._

// Branch Prediction(static or dynamic)

trait BranchPrediction
object disable extends BranchPrediction
object static extends BranchPrediction
object dynamic extends BranchPrediction

case class BranchPredictionLine()(implicit p:RiscvCoreConfig) extends Bundle{
    
}