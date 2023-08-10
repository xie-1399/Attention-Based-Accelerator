package CPU.Nax.misc
import spinal.lib._
import spinal.core._


// MMU Unit
case class MmuStorageLevel(id:Int,ways:Int,depth:Int){
  assert(isPow2(depth))
}

case class MmuStorageParameter(levels: Seq[MmuStorageLevel],
                               priority:Int)

case class MmuPortParameter(var readAt:Int,var hitsAt:Int,var ctrlAt : Int,var rspAt : Int)

case class MmuLevel(virtualWidth:Int,physicalWidth:Int,virtualOffset:Int,physicalOffset:Int,entryOffset:Int){ //page table level
  val virtualRange = virtualOffset + virtualWidth -1 downto virtualOffset
  val entryRange = entryOffset + physicalWidth -1 downto entryOffset
  val physicalRange = physicalOffset + physicalWidth -1 downto physicalOffset

  def vpn(address : UInt) : UInt = address(virtualRange)  //get the vpn address
}

case class MmuSpec(levels: Seq[MmuLevel],entryBytes : Int,virtualWidth:Int,physicalWidth:Int,satpMode : Int)  //Todo with satpMpde

//know about sv32 and sv39 first
object MmuSpec{
  val sv32 = MmuSpec(
    levels = List(
      MmuLevel(virtualWidth = 10, physicalWidth = 10, virtualOffset = 12, physicalOffset = 12, entryOffset = 10),
      MmuLevel(virtualWidth = 10, physicalWidth = 10, virtualOffset = 22, physicalOffset = 22, entryOffset = 20)
    ),
    entryBytes = 4,
    virtualWidth = 32,
    physicalWidth = 32,
    satpMode = 1
  )
  val sv39 = MmuSpec(
    levels = List(
      MmuLevel(virtualWidth = 9, physicalWidth = 9, virtualOffset = 12, physicalOffset = 12, entryOffset = 10),
      MmuLevel(virtualWidth = 9, physicalWidth = 9, virtualOffset = 21, physicalOffset = 21, entryOffset = 19),
      MmuLevel(virtualWidth = 9, physicalWidth = 26, virtualOffset = 30, physicalOffset = 30, entryOffset = 28)
    ),
    entryBytes = 8,
    virtualWidth = 39,
    physicalWidth = 56,
    satpMode = 8
  )
}

class MmuPlugin extends {

}
