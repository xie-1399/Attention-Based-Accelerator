package CPU.Nax.Interface
import spinal.core._
import spinal.lib._
import spinal.core.fiber._
import spinal.lib.pipeline._
import CPU.Nax.utils._
import CPU.Nax.Global._

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

//-------------------------------------------------------------------
// Address Translation Service
trait AddressTranslationPortUsage
object AddressTranslationPortUsage{
  //the address translation happen on Fetch and Memory
  object FETCH extends AddressTranslationPortUsage
  object LOAD_STORE extends AddressTranslationPortUsage
}

class AddressTranslationRsp(s : AddressTranslationService, wakesCount : Int , val rspStage : Stage ,val wayCount : Int) extends Area{
  val keys = new Area{
    setName("MMU")
    val TRANSLATED = Stageable(UInt(PHYSICAL_WIDTH bits))
    val IO = Stageable(Bool())
    val REDO = Stageable(Bool())
    val ALLOW_READ, ALLOW_WRITE, ALLOW_EXECUTE = Stageable(Bool())
    val PAGE_FAULT = Stageable(Bool())
    val ACCESS_FAULT = Stageable(Bool())
    val WAYS_OH = Stageable(Bits(wayCount bits))
    val WAYS_PHYSICAL = Stageable(Vec.fill(wayCount)(UInt(PHYSICAL_WIDTH bits)))
    val BYPASS_TRANSLATION = Stageable(Bool())
  }
  val wake = Bool()
  val pipelineLock = Lock().retain()
}

trait AddressTranslationService extends Service with LockedImpl{
  def newStorage(pAny:Any) : Any
  def newTranslationPort(stages:Seq[Stage],
                         preAddress : Stageable[UInt],
                         allowRefill : Stageable[Bool],
                         usage: AddressTranslationPortUsage,
                         portSpec:Any,
                         storageSpec:Any):AddressTranslationRsp
  def withTranslation : Boolean
  def invalidatePort : FlowCmdRsp[NoData,NoData]
}
//-------------------------------------------------------------------

object PerformanceCounterService{
  val ICACHE_REFILL = 1
  val DCACHE_REFILL = 2
  val DCACHE_WRITEBACK = 3
  val BRANCH_MISS = 4
}