package org.adridadou.openlaw.oracles

import org.adridadou.openlaw.result.{Result, Success}
import org.adridadou.openlaw.vm.{OpenlawVm, OpenlawVmEvent}

import scala.reflect.ClassTag

trait OpenlawOracle[R <: OpenlawVmEvent] {
  def incoming(vm:OpenlawVm, incomingData:R): Result[OpenlawVm]

  def executeIfPossible(vm:OpenlawVm, event:OpenlawVmEvent)(implicit classTag:ClassTag[R]): Result[OpenlawVm] = event match {
    case typedEvent:R if shouldExecute(typedEvent) => incoming(vm, typedEvent)
    case _ => Success(vm)
  }

  def shouldExecute(event:OpenlawVmEvent):Boolean
}