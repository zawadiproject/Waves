package com.wavesplatform.network

import com.wavesplatform.utils.{Execution, ScorexLogging}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

@Sharable
class ChannelClosedHandler private extends ChannelHandlerAdapter with ScorexLogging {

  private val scheduler = Execution.scheduler(log.error("Error in closed-channels-handler: ", _))

  private val closedChannelsSubject = ConcurrentSubject.publish[Channel](scheduler)

  override def handlerAdded(ctx: ChannelHandlerContext): Unit = {
    ctx.channel().closeFuture().addListener((cf: ChannelFuture) => closedChannelsSubject.onNext(cf.channel()))
    super.handlerAdded(ctx)
  }

  def shutdown(): Unit = {
    closedChannelsSubject.onComplete()
  }
}

object ChannelClosedHandler {
  def apply(): (ChannelClosedHandler, Observable[Channel]) = {
    val h = new ChannelClosedHandler()
    (h, h.closedChannelsSubject)
  }
}
