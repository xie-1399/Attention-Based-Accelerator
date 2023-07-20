package Lib.Math.Int
import spinal.core._
import spinal.lib._

//implement the Int Sqrt calculation
//the method is : http://www.cppblog.com/QUIRE-0216/archive/2008/01/23/41714.html


class IntSqrt(inputWidth:Int,outputWidth:Int) extends Component {
  val io = new Bundle{
      val option = slave Flow(UInt(inputWidth bits))
      val results = master Flow (UInt(outputWidth bits))
  }


}


