package com.wavesplatform.api.http.alias

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http._
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup

import scala.concurrent.ExecutionContext

case class AliasBroadcastApiRoute(settings: RestAPISettings, utx: UtxPool, allChannels: ChannelGroup, executionContext: ExecutionContext)
    extends ApiRoute
    with BroadcastRoute {

  override implicit val ec: ExecutionContext = executionContext

  override val route = pathPrefix("alias" / "broadcast") {
    signedCreate
  }

  def signedCreate: Route = (path("create") & post) {
    json[SignedCreateAliasV1Request] { aliasReq =>
      doBroadcast(aliasReq.toTx)
    }
  }
}
