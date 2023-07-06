package MicroRV32.core

class RVConfig(Xlen:Int,withCompressed:Boolean = false) {
  val xlen = Xlen
  val compressExtension = withCompressed
}
