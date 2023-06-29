package CNN

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
//try to use some simple instructions to control the Conv

class Instruction(addr:Int) extends Component {
  //Todo instructions
  val io = new Bundle{
    val regData = slave(AxiLite4(log2Up(1 MiB),32)) //read instruction
    //val convIR
  }
}
