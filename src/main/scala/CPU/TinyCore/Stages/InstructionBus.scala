package CPU.TinyCore.Stages

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import CPU.TinyCore._
import lib.Sim.SpinalSim.PrefixComponent

import scala.collection.mutable.ArrayBuffer
//some bundles about the Instruction Bus

//Todo add branch prediction
case class CoreInstructionCmd()(implicit p: RiscvCoreConfig) extends Bundle{
  val pc = UInt(p.addressWidth bits)
}

case class CoreInstructionRsp()(implicit p:RiscvCoreConfig) extends Bundle{
  val instruction = Bits(32 bits)
  val pc = UInt(p.addressWidth bits)
  //val branchCacheLine = null   //Todo if branch is dynamic -> pc jump
}

object InstructionBus{
  //set instruction bus config
  def getAxi4Config(p: RiscvCoreConfig) = Axi4Config(
    addressWidth = p.addressWidth,
    dataWidth = 32,
    useId = false,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useQos = false,
    useLen = false,
    useResp = false,
    useSize = false
  )
}

case class InstructionBus()(implicit p:RiscvCoreConfig) extends Bundle with IMasterSlave{
  val cmd = Stream(CoreInstructionCmd())
  //val branchCachePort = null Todo
  val rsp = Stream(CoreInstructionRsp())

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

  def toAxi4ReadOnly():Axi4ReadOnly = {
    val axi4Config = InstructionBus.getAxi4Config(p)
    val bus = Axi4ReadOnly(axi4Config)
    val pendingCmd = RegInit(False)
    pendingCmd := bus.readCmd.fire || (pendingCmd && !bus.readRsp.valid)  //the bus has sent read cmd
    val haltCmd = rsp.isStall || (pendingCmd && !bus.readRsp.valid)

    bus.readCmd.valid := cmd.valid && !haltCmd
    bus.readCmd.addr := cmd.pc(bus.readCmd.addr.getWidth -1 downto 2) @@ U"00"  //address should align
    bus.readCmd.prot := "110"
    bus.readCmd.cache := "1111"

    cmd.ready := bus.readCmd.ready && !haltCmd

    //Internal fifo(just one depth)
    val backupFifoIn = Stream(CoreInstructionRsp())
    backupFifoIn.valid := bus.readRsp.valid
    backupFifoIn.instruction := bus.readRsp.data
    backupFifoIn.pc := RegNextWhen(cmd.pc,cmd.ready)
    rsp </< backupFifoIn //(that.s2mPipe)

    bus.readRsp.ready := True
    bus
  }
}