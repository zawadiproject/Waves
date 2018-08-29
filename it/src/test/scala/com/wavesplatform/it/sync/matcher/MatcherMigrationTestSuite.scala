package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{TransferSending, _}
import com.wavesplatform.state.ByteStr
import org.scalatest.concurrent.Eventually
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import scorex.transaction.assets.exchange.{AssetPair, OrderType}

import scala.concurrent.duration._
import scala.util.Random

class MatcherMigrationTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker
    with TransferSending
    with Eventually {

  import MatcherMigrationTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs
  private def matcherNode                         = nodes.head
  private def aliceNode                           = nodes(1)

  "check order execution" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceNode.address, "DisconnectCoin", "Alice's coin for disconnect tests", someAssetAmount, 8, reissuable = false, issueFee).id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    val aliceBalance = aliceNode.accountBalances(aliceNode.address)._2
    val t1           = aliceNode.transfer(aliceNode.address, matcherNode.address, aliceBalance - fee - 250000, fee, None, None).id
    nodes.waitForHeightAriseAndTxPresent(t1)

    val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)
    // check assets's balances
    "make order and after matcher's restart try to cancel it" in {
      // Alice places sell order
      val aliceOrder = matcherNode
        .placeOrder(aliceNode, aliceWavesPair, OrderType.SELL, 3000000, 3000000)
      aliceOrder.status shouldBe "OrderAccepted"
      val firstOrder = aliceOrder.message.id

      // check order status
      matcherNode.orderStatus(firstOrder, aliceWavesPair).status shouldBe "Accepted"

      // sell order should be in the aliceNode orderbook
      matcherNode.fullOrderHistory(aliceNode).head.status shouldBe "Accepted"

      // reboot matcher's node
      docker.killStartContainerAndMigrateMatcher(dockerNodes().head)
      Thread.sleep(60.seconds.toMillis)

      val height = nodes.map(_.height).max

      matcherNode.waitForHeight(height + 1, 40.seconds)
      matcherNode.orderStatus(firstOrder, aliceWavesPair).status shouldBe "Accepted"
      matcherNode.fullOrderHistory(aliceNode).head.status shouldBe "Accepted"

      matcherNode.tradableBalance(aliceNode, aliceWavesPair)

    }
  }
}

object MatcherMigrationTestSuite {
  import ConfigFactory._
  import com.wavesplatform.it.NodeConfigs._

  private val minerDisabled = parseString("waves.miner.enable = no")
  private val matcherConfig = parseString(s"""
                                             |waves.matcher {
                                             |  enable = yes
                                             |  account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
                                             |  bind-address = "0.0.0.0"
                                             |  order-match-tx-fee = 300000
                                             |  balance-watching.enable = yes
                                             |}""".stripMargin)

  private val Configs: Seq[Config] = (Default.last +: Random.shuffle(Default.init).take(3))
    .zip(Seq(matcherConfig, minerDisabled, minerDisabled, empty()))
    .map { case (n, o) => o.withFallback(n) }
}
