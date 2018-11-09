package com.wavesplatform.network

import com.wavesplatform.utils.{Execution, ScorexLogging}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext}
import monix.execution.Scheduler
import monix.reactive.Observable

@Sharable
class DiscardingHandler(blockchainReadiness: Observable[Boolean]) extends ChannelDuplexHandler with ScorexLogging {

  private implicit val scheduler: Scheduler = Execution.scheduler(log.error("Error in discarding-handler: ", _))
  private val lastReadiness                 = lastObserved(blockchainReadiness)

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case RawBytes(code, _) if code == TransactionSpec.messageCode && !lastReadiness().contains(true) =>
      log.trace(s"${id(ctx)} Discarding incoming message $code")
    case _ => super.channelRead(ctx, msg)
  }
}
