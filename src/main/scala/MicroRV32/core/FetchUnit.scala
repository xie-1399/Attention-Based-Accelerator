package MicroRV32.core
import spinal.core._
import spinal.lib._


//Todo support the compressed instruction
class FetchUnit(rvConfig: CoreConfig) extends Component {
  val Xlen = rvConfig.xlen

  val io = new Bundle{
    val data = in Bits(const.instructionWidth bits)
    val sample = in Bool() //only on sampler can get the instruction buffer
    val instruction = out Bits(const.instructionWidth bits)
    val pc = in UInt(Xlen bits)
    val pcIncrement = out(UInt(3 bits))
    //val compressed = rvConfig.compressExtension generate()
  }

  val instructionBuffer = Reg(Bits(const.instructionWidth bits)) init(0)

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
  spinalConfig.setconfig(new FetchUnit(CoreConfig()))
}