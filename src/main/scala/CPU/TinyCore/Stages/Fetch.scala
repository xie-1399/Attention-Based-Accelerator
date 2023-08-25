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
    val pcLoad = slave Flow(UInt(p.pcWidth bits)) //for the branch prediction
    val haltCpu = in Bool()
    val outInst = master Stream FetchOutput()
  }

  noIoPrefix()
  //before Fetch(send instruction request to iCmd)
  val preFetch = new Area {
    val haltCpu = io.haltCpu
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
    when(io.pcLoad.valid){
      pcNext := io.pcLoad.payload
    }

    val resetDone = RegNext(True) init(False)
    io.iCmd.valid := resetDone && !haltCpu
    io.iCmd.pc := pcNext
    when(io.iCmd.fire || io.pcLoad.valid){
      pc := pcNext
    }
    when(io.iCmd.fire){
      inc := True
    }elsewhen(io.pcLoad.valid){
      inc := False
    }
  }
  //Fetch stage(get the rsp instruction)
  val Fetch = new Area {
    val outInst = io.outInst
    val throwIt = False
    val flush = False
    when(flush){
      throwIt := True
    }
    //store the icmd request until the irsp comes
    val pendingPrefetch = CounterUpDown(
      stateCount = 4,
      incWhen = io.iCmd.fire,
      decWhen = io.iRsp.fire
    )
    when(pendingPrefetch === 3){
      io.iCmd.valid := False
    }

    //flush the continue rsp (maybe more than one)
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
  }
}