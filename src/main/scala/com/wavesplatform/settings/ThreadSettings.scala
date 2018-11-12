package com.wavesplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

final case class ThreadSettings(nettyBoss: Int, nettyWorker: Int, sigVerify: Int)

object ThreadSettings {
  def fromConfig(config: Config): ThreadSettings = {
    val threadConfig = config.getConfig("waves.threads")
    ThreadSettings(
      threadConfig.as[Int]("netty.boss"),
      threadConfig.as[Int]("netty.worker"),
      threadConfig.as[Int]("signature-verification-pool")
    )
  }
}
