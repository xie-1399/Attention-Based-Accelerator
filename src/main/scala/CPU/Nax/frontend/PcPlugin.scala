package CPU.Nax.frontend
import CPU.Nax.utils._
import CPU.Nax.Interface._
import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer
import CPU.Nax.Fetch._
import CPU.Nax.Global._

//the pc Plugin show the PC value
//notice : support compressed / support jump

class PcPlugin(var resetVector:BigInt = 0x80000000l,fecthPcIncAt:Int = 1) extends Plugin with JumpService{

  case class JumpSpec(interface : Flow[JumpCmd], priority:Int , aggregationPriority:Int)
  val jumpsSpec = ArrayBuffer[JumpSpec]()

  override def createJumpInterface(priority: Int, aggregationPriority: Int = 0): Flow[JumpCmd] = {
    jumpsSpec.addRet(JumpSpec(Flow(JumpCmd(PC_WIDTH)),priority,aggregationPriority)).interface
  }

  //get the Fetch Service
  val setup = create early new Area {
    val fetch = getService[FetchPlugin]
    fetch.lock.retain()
  }

  val logic = create late new Area{
    //build the pc plugin here
    val stage = setup.fetch.getStage(0)  //set as first stage
    val fetch = getService[FetchPlugin]
    val pipeline = fetch.getPipeline()

    val sliceRange = SLICE_RANGE  // 2 -> 1 (1 and 2)
    val jump = new Area {
      val sortedByStage = jumpsSpec.sortWith(_.priority > _.priority) //sort with priority
      val valids = sortedByStage.map(_.interface.valid)
      val cmds = sortedByStage.map(_.interface.payload)
      val oh = OHMasking.firstV2(valids.asBits) //Todo

      val target = PC()
      //set up the jump value(pc)
      val pcLoad = Flow(JumpCmd(pcWidth = widthOf(PC)))
      pcLoad.valid := jumpsSpec.map(_.interface.valid).orR
      pcLoad.payload.pc := target

    }

  }

  val init = new Area {
    val requests = getServicesOf[InitCycles]
    val request = ( 0 +: requests.map(_.initCycles)).max //get max initcycles
    val counter = Reg(UInt(log2Up(request)+ 1 bits)) init(0)
    val booted = counter.msb
    counter := counter + U(!booted)
  }

  //if there is no jump cmd -> fetch pc increase


}
