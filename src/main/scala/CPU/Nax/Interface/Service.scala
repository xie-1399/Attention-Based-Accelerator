package CPU.Nax.Interface
import spinal.core._
import spinal.lib._
import spinal.core.fiber._
import CPU.Nax.utils._

//Define the Plugin Service here
trait LockedService{
  def retain()
  def release()
}

trait LockedImpl extends LockedService{
  val lock = Lock()
  override def release(): Unit = lock.release()
  override def retain(): Unit =  lock.retain()
}

object JumpService{
  //order by number (inorder)
  object Priorities{
    //define jump priorities here
    def FETCH_WORD(stage:Int,prediction:Boolean) = stage * 2 + (if(prediction) -1 else 0)
    val ALIGNER = 90
    val DECODE_PREDICTION = 100
    val COMMIT_RESCHEDULE = 200
    val COMMIT_TRAP = 201
  }
}

case class JumpCmd(pcWidth:Int) extends Bundle {
  //jump Bundle
  val pc = UInt(pcWidth bits)
}

trait JumpService extends Service{
  //create a jump interface with flow jump cmd
  def createJumpInterface(priority:Int,aggregationPriority:Int = 0):Flow[JumpCmd]
}

trait InitCycles extends Service{
  def initCycles : Int
}
