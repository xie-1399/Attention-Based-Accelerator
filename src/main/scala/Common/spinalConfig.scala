package Common

import spinal.core._

object spinalConfig{
  case class RtlConfig(path:String = "rtl",frequency : Int = 50, hdl: SpinalMode = Verilog){
    def setconfig = SpinalConfig(mode = hdl,targetDirectory = path,defaultClockDomainFrequency = FixedFrequency( frequency MHz))
  }
}
