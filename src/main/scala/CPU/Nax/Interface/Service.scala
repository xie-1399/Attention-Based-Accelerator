package CPU.Nax.Interface
import spinal.core._
import spinal.lib._
import spinal.core.fiber._

//Define the Plugin Service here

trait LockedService{
  def retain()
  def release()
}

trait LockedImpl extends LockedService{
  val lock = Lock()
  override def release(): Unit = lock.release()
  override def retain(): Unit =  lock.retain()
}

class Service {

}
