package CPU.Nax.misc

import CPU.Nax.utils.Plugin
import CPU.Nax.Interface.{AddressTranslationRsp,AddressTranslationService,AddressTranslationPortUsage}
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.{Stage, Stageable}

import scala.collection.mutable.ArrayBuffer

//static translation(the translated address == pre address)

case class StaticAddressTranslationParameter(rspAt:Int)

class StaticAddressTranslation(var physicalWidth: Int,
                               var ioRange: UInt => Bool,
                               var fetchRange: UInt => Bool) extends Plugin with AddressTranslationService {

  override def withTranslation: Boolean = true
  override def newStorage(pAny: Any): Any = "just static translation"
  case class Spec(stages:Seq[Stage],preAddress:Stageable[UInt],p:StaticAddressTranslationParameter , rsp:AddressTranslationRsp)
  val specs = ArrayBuffer[Spec]()


  override def newTranslationPort(stages: Seq[Stage], preAddress: Stageable[UInt], allowRefill: Stageable[Bool], usage: AddressTranslationPortUsage, portSpec: Any, storageSpec: Any): AddressTranslationRsp = {
    val p = portSpec.asInstanceOf[StaticAddressTranslationParameter]
    specs.addRet(new Spec(stages,preAddress = preAddress, p = p, rsp = new AddressTranslationRsp(this,0,stages(p.rspAt), wayCount = 0){
      import rspStage._
      import keys._

    REDO := False
    TRANSLATED := preAddress  //just set the Translated == Pre
    IO := ioRange(TRANSLATED)
    ALLOW_EXECUTE := True
    ALLOW_READ := True
    ALLOW_WRITE := True
    PAGE_FAULT := False
    ACCESS_FAULT := False
    wake := True

    ALLOW_EXECUTE clearWhen (!fetchRange(TRANSLATED))
    pipelineLock.release()
    })).rsp
  }

  override def invalidatePort: FlowCmdRsp[NoData, NoData] = setup.invalidatePort
  val setup = create early new Area{
    val invalidatePort = FlowCmdRsp().setIdleAll()
    invalidatePort.rsp.valid setWhen(RegNext(invalidatePort.cmd.valid) init(False))
  }

  val logic = create late new Area{
    lock.await()
  }

}
