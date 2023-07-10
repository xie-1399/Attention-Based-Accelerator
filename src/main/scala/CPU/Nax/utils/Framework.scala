package CPU.Nax.utils

import spinal.core._
import spinal.core.fiber.{Handle, Lock}
import spinal.lib.pipeline._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, classTag}


trait Service{}

//all plugins has it's own service
trait Plugin extends Area with Service{
  this.setName(ClassName(this))
  def withPrefix(prefix : String) = setName(prefix + "_" + getName())

  val framework = Handle[Framework]()
  val configsHandles = ArrayBuffer[Handle[_]]()
  val earlyHandles = ArrayBuffer[Handle[_]]()

  def create = new {
    def config[T](body : => T) : Handle[T] = {
      val h = Handle{
        framework.buildLock.retain()
        val ret = framework.rework {
          framework.configLock.await()
          body
        }
        framework.buildLock.release()
        ret
      }
      configsHandles += h
      h
    }
    def early[T](body : => T) : Handle[T] = {
      val h = Handle{
        framework.buildLock.retain()
        val ret = framework.rework {
          framework.earlyLock.await()
          body
        }
        framework.buildLock.release()
        ret
      }
      earlyHandles += h
      h
    }
    def late[T](body : => T) : Handle[T] = {
      Handle{
        framework.buildLock.retain()
        val ret = framework.rework {
          framework.lateLock.await()
          body
        }
        framework.buildLock.release()
        ret
      }
    }
  }
  //all plugins can get it's Service
  def getSubServices() : Seq[Service] = Nil
  def isServiceAvailable[T <: Service : ClassTag] : Boolean = framework.getServicesOf[T].nonEmpty
  def getService[T <: Service : ClassTag] : T = framework.getService[T]
  def getServicesOf[T <: Service : ClassTag] : Seq[T] = framework.getServicesOf[T]
  def getServiceOption[T <: Service : ClassTag] : Option[T] = if(isServiceAvailable[T]) Some(framework.getService[T]) else None
  def findService[T <: Service : ClassTag](filter : T => Boolean) = getServicesOf[T].find(filter).get
}

class FrameworkConfig(){
  val plugins = ArrayBuffer[Plugin]()
}

//The Framework only use in the Top level to control all plugins
class Framework(val plugins : Seq[Plugin]) extends Area{
  val services = plugins ++ plugins.flatMap(_.getSubServices()) //get all services
  val configLock = Lock()
  val earlyLock = Lock()
  val lateLock = Lock()
  val buildLock = Lock()

  configLock.retain()
  earlyLock.retain()
  lateLock.retain()

  plugins.foreach(_.framework.load(this)) // Will schedule all plugins early tasks

  configLock.release()
  plugins.foreach(_.configsHandles.foreach(_.await()))

  earlyLock.release()
  plugins.foreach(_.earlyHandles.foreach(_.await()))
  lateLock.release()


  def getServicesOf[T <: Service : ClassTag] : Seq[T] = {
    val clazz = (classTag[T].runtimeClass)
    val filtered = services.filter(o => clazz.isAssignableFrom(o.getClass))
    filtered.asInstanceOf[Seq[T]]
  }
  def getService[T <: Service : ClassTag] : T = {
    val filtered = getServicesOf[T]
    filtered.length match {
      case 0 => throw new Exception(s"Can't find the service ${classTag[T].runtimeClass.getName}")
      case 1 => filtered.head
      case _ => throw new Exception(s"Found multiple instances of ${classTag[T].runtimeClass.getName}")
    }
  }

  def getServiceWhere[T: ClassTag](filter : T => Boolean) : T = {
    val clazz = (classTag[T].runtimeClass)
    val filtered = services.filter(o => clazz.isAssignableFrom(o.getClass) && filter(o.asInstanceOf[T]))
    assert(filtered.length == 1, s"??? ${clazz.getName}")
    filtered.head.asInstanceOf[T]
  }

  def getServices = services
}
