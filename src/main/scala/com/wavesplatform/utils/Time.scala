package com.wavesplatform.utils

import java.net.InetAddress

import monix.eval.Task
import monix.execution.Scheduler
import org.apache.commons.net.ntp.NTPUDPClient

import scala.concurrent.duration.DurationInt

trait Time {
  def correctedTime(): Long

  def getTimestamp(): Long
}

class TimeImpl extends Time with ScorexLogging with AutoCloseable {

  private val offsetPanicThreshold = 1000000L
  private val ExpirationTimeout    = 60.seconds
  private val RetryDelay           = 10.seconds
  private val ResponseTimeout      = 10.seconds
  private val NtpServer            = "pool.ntp.org"

  private implicit val scheduler: Scheduler = Execution.scheduler(log.error("Error in time-impl: ", _))

  private val client = new NTPUDPClient()
  client.setDefaultTimeout(ResponseTimeout.toMillis.toInt)

  @volatile private var offset = 0L
  private val updateTask: Task[Unit] = {
    def newOffsetTask: Task[Long] = Task {
      try {
        client.open()
        val info = client.getTime(InetAddress.getByName(NtpServer))
        info.computeDetails()
        val offset = info.getOffset
        if (Math.abs(offset) > offsetPanicThreshold)
          throw new Exception("Offset is suspiciously large")
        else offset
      } finally {
        client.close()
      }
    }

    newOffsetTask
      .flatMap { newOffset =>
        log.trace(s"Adjusting time with $newOffset milliseconds.")
        offset = newOffset
        updateTask.delayExecution(ExpirationTimeout)
      }
      .onErrorRecover {
        case t: Throwable =>
          log.error("Problem with NTP: ", t)
          updateTask.delayExecution(RetryDelay)
      }
  }

  def correctedTime(): Long = System.currentTimeMillis() + offset

  private var txTime: Long = 0

  def getTimestamp(): Long = {
    txTime = Math.max(correctedTime(), txTime + 1)
    txTime
  }

  private val taskHandle = updateTask.runAsyncLogErr

  override def close(): Unit = {
    log.info("Shutting down Time")
    taskHandle.cancel()
  }
}

object NTP extends TimeImpl
