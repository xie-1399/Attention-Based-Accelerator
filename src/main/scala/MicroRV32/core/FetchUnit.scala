package MicroRV32.core
import spinal.core._
import spinal.lib._


//Todo support the compressed instruction
class FetchUnit(rvConfig: RVConfig) extends Component {
  val Xlen = rvConfig.xlen
  val io = new Bundle{
    val data = in Bits(Xlen bits)
    val sample = in Bool() //only on sampler can get the instruction buffer
    val instruction = out Bits(Xlen bits)
    val pc = in UInt(Xlen bits)
    val pcIncrement = out(UInt(Xlen bits))
    //val compressed = rvConfig.compressExtension generate()
  }

  val instructionBuffer = Reg(Bits(Xlen bits)) init(0)

  //without compress
  if(!rvConfig.compressExtension){
    when(io.sample){
      instructionBuffer := io.data
    }
    io.instruction := instructionBuffer
    io.pcIncrement := 4
  }

  //with compress
  else{
    ???
  }

}

object FetchUnit extends App{
  SpinalVerilog(new FetchUnit(new RVConfig(32)))
}