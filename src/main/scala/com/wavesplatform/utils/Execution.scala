package com.wavesplatform.utils

import monix.execution.{Scheduler, UncaughtExceptionReporter}

import scala.concurrent.ExecutionContext

object Execution {
  implicit lazy val globalEC: ExecutionContext = scala.concurrent.ExecutionContext.global
  implicit lazy val globalScheduler: Scheduler = Scheduler(globalEC)

  def scheduler(reporter: UncaughtExceptionReporter): Scheduler = Scheduler(globalEC, reporter)
}
