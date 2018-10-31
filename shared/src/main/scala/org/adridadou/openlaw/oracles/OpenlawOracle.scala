package org.adridadou.openlaw.oracles

import org.adridadou.openlaw.vm.{OpenlawVm, OpenlawVmEvent}

import scala.reflect.ClassTag

trait OpenlawOracle[R <: OpenlawVmEvent] {
  def incoming(vm:OpenlawVm, incomingData:R):Either[String, OpenlawVm]

  def executeIfPossible(vm:OpenlawVm, event:OpenlawVmEvent)(implicit classTag:ClassTag[R]):Either[String, OpenlawVm] = event match {
    case typedEvent:R if shouldExecute(typedEvent) => incoming(vm, typedEvent)
    case _ => Right(vm)
  }

  def shouldExecute(event:OpenlawVmEvent):Boolean
}