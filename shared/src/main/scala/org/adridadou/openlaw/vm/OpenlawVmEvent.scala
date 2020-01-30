package org.adridadou.openlaw.vm

import scala.reflect.ClassTag

trait OpenlawVmEvent {
  def typeIdentifier: String
  def serialize: String
  protected def className[T]()(implicit cls: ClassTag[T]): String =
    cls.runtimeClass.getName
}

trait OpenlawVmInitEvent extends OpenlawVmEvent
