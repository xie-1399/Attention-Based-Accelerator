package CPU.Nax.fetch

import CPU.Nax.Fetch._
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable
/*this is about how to implement Fetch Cache
* ICache is ReadOnly
* */
case class FetchL1Cmd(physicalWidth:Int) extends Bundle{
  //fetch ir from L1 Cmd
  val address = UInt(physicalWidth bits)
  val io = Bool()
}

case class FetchL1Rsp(dataWidth:Int) extends Bundle {
  //return fetch error
  val data = Bits(dataWidth bits)
  val error = Bool()
}

case class FetchL1Bus(physicalWidth:Int,
                      dataWidth:Int,
                      lineSize:Int, //Cache line size
                      withBackPresure:Boolean) extends Bundle with IMasterSlave{
  val cmd = Stream(FetchL1Cmd(physicalWidth))
  val rsp = Stream(FetchL1Rsp(dataWidth))
  val beatCount = lineSize * 8 / dataWidth
  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

  //Composite can set the name Area and can get the self Pro //spilt (!_.io)
  //split io cmd and fetch cmd
  def split(selGen:FetchL1Cmd => Bool):(FetchL1Bus,FetchL1Bus) = new Composite(this,"split"){
    //clone into two bus requests
    val bus0,bus1 = cloneOf(self)
    val selCmd = selGen(cmd)
    val selRsp = RegNextWhen(selCmd,cmd.valid)

    bus1.cmd.valid := cmd.valid && selCmd
    bus0.cmd.valid := cmd.valid && !selCmd

    bus0.cmd.payload := cmd.payload
    bus1.cmd.payload := cmd.payload

    //like mux(selCmd,bus1.cmd.ready,bus0.cmd.ready)
    cmd.ready := selCmd ? bus1.cmd.ready | bus0.cmd.ready
    rsp.valid := bus1.rsp.valid || bus0.rsp.valid
    rsp.payload := selRsp ? bus1.rsp.payload | bus0.rsp.payload
    bus1.rsp.ready := rsp.ready
    bus0.rsp.ready := rsp.ready
    val ret = (bus0,bus1)
  }.ret

  def ioSplit():(FetchL1Bus,FetchL1Bus) = split(!_.io) //bus 0 -> get io request

  //Todo resizer
}

case class FetchBypassSpec(stageId : Int) extends Area{
  val valid = Stageable(Bool())
  val data = Stageable(WORD)
}



class FetchCachePlugin {

}


