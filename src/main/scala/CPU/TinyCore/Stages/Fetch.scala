package CPU.TinyCore.Stages

import spinal.core._
import spinal.lib._
import CPU.TinyCore._
import CPU.TinyCore.Misc.Config._
import lib.Sim.SpinalSim.PrefixComponent
case class FetchOutput()(implicit p:RiscvCoreConfig) extends Bundle{
  //fetch output bundle
  val pc = UInt(p.pcWidth bits)
  val instruction = Bits(32 bits)
  //Todo add Branch
}

class Fetch(implicit p : RiscvCoreConfig) extends Component {
  val io = new Bundle{
    //instruction bus bundle
    val iCmd = master Stream(CoreInstructionCmd()) //send the cmd
    val iRsp = slave Stream(CoreInstructionRsp()) //get the rsp
  }
  noIoPrefix()
  //before Fetch(send instruction request to iCmd)
  val preFetch = new Area {
    val haltCpu = False
    val pc = Reg(UInt(p.pcWidth bits)) init(U(p.startAddress,p.pcWidth bits)) //set initial pc value
    val inc = RegInit(False)

    val pcNext = if(p.fastFetchCmdPcCalculation){
      val pcPlus4 = pc + U(4)
      pcPlus4.addAttribute("keep")
      Mux(inc,pcPlus4,pc)
    }else{
      pc + Mux(inc,U(4),U(0))
    }

    // branch the pc
    val pcLoad = Flow(pc)
    when(pcLoad.valid){
      pcNext := pcLoad.payload
    }
    val resetDone = RegNext(True) init(False)
    io.iCmd.valid := resetDone && !haltCpu
    io.iCmd.pc := pcNext
    when(io.iCmd.fire || pcLoad.valid){
      pc := pcNext
    }
    when(io.iCmd.fire){
      inc := True
    }elsewhen(pcLoad.valid){
      inc := False
    }
  }
  //Fetch stage(get the rsp instruction)
  val Fetch = new Area {

    val outInst = Stream(FetchOutput())
    val throwIt = False
    val flush = False
    when(flush){
      throwIt := True
    }
    val pendingPrefetch = CounterUpDown(
      stateCount = 4,
      incWhen = io.iCmd.fire,
      decWhen = io.iRsp.fire
    )
    when(pendingPrefetch === 3){
      io.iCmd.valid := False  //Todo
    }
    val throwRemaining = Reg(UInt(2 bits)) init(0)
    val throwNextIRsp = throwRemaining =/= 0
    when(throwNextIRsp && io.iRsp.fire){
      throwRemaining := throwRemaining - 1
    }
    when(throwIt){
      throwRemaining := pendingPrefetch - io.iRsp.valid.asUInt
    }
    //connect ready and valid
    outInst.arbitrationFrom(io.iRsp.throwWhen(throwIt || throwNextIRsp))
    outInst.pc := io.iRsp.pc
    outInst.instruction := io.iRsp.instruction
    outInst.ready := True //Todo
  }

  //Todo no pc load(for the branch)
  preFetch.pcLoad.valid := False
  preFetch.pcLoad.payload := 0
}