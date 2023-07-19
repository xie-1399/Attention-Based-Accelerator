package CPU.Nax.fetch

import CPU.Nax.Fetch._
import CPU.Nax.Global._
import CPU.Nax.Interface._
import CPU.Nax.utils._
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

//the pc Plugin show the PC value
//notice : support compressed / support jump

class PcPlugin(var resetVector:BigInt = 0x80000000l,fetchPcIncAt:Int = 1) extends Plugin with JumpService{

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

  val logic = create late new Area {
    //build the pc plugin here
    val stage = setup.fetch.getStage(0) //set as first stage
    val fetch = getService[FetchPlugin]
    val pipeline = fetch.getPipeline()

    val sliceRange = SLICE_RANGE // 2 -> 1 (1 and 2)
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

    val init = new Area {
      val requests = getServicesOf[InitCycles]
      val request = (0 +: requests.map(_.initCycles)).max //get max initcycles
      val counter = Reg(UInt(log2Up(request) + 1 bits)) init (0)
      val booted = counter.msb
      counter := counter + U(!booted)
    }

    //if there is no jump cmd -> fetch pc increase
    val fetchPC = new Area {
      val output = Stream(PC)
      //pc reg for the test wave
      val pcReg = Reg(PC) init (resetVector) addAttribute (Verilator.public)
      val correction = False
      val correctionReg = RegInit(False) setWhen (correction) clearWhen (output.fire)
      val corrected = correction || correctionReg
      val pcRegPropagate = False //Todo
      //no valid request
      val inc = RegInit(False) clearWhen (correction || pcRegPropagate) setWhen (output.fire) clearWhen (!output.valid && output.ready)
      val pc = pcReg + (U(inc) << sliceRange.high + 1) //increase 8 or 4


      val flushed = False
      when(inc){
        pc(sliceRange) := 0
      }
      //a jump cmd happens load the pc and flush it
      when(jump.pcLoad.valid){
        correction := True
        pc := jump.pcLoad.pc
        flushed := True
      }
      //after init cycles finish start
      when(init.booted && (output.ready || correction || pcRegPropagate)){
        pcReg := pc
      }
      pc(0) := False
      if(!RVC) pc(1) := False

      //define the halt signal
      val fetchHalt = False
      output.valid := !fetchHalt && init.booted
      output.payload := pc
    }
    //set the stage ready to the fetch pc ready
    fetchPC.output.ready := stage.isReady
    stage.valid := fetchPC.output.valid
    stage(FETCH_PC) := fetchPC.output.payload //set Fetch_PC reg

    //get the next pc
    val pcNext = new Area{
      val stage = fetch.getStage(fetchPcIncAt)
      stage(FETCH_PC_INC) := stage(FETCH_PC) + (1 << sliceRange.high + 1 )  // plus 8
      stage(FETCH_PC_INC)(sliceRange.high downto 0) := 0
    }
    //Todo about sliceRange.high always 2
    setup.fetch.lock.release()
  }
}
