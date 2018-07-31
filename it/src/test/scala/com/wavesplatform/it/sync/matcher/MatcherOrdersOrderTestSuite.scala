package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.util.Random

class MatcherOrdersOrderTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {

  import MatcherOrdersOrderTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  private val sellAmount = 50

  "Check cross ordering between Alice and Bob " - {
    val price = 2.waves

    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceNode.address, "AliceCoin", "AliceCoin for matcher's tests", AssetQuantity, 8, reissuable = false, 100000000L).id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    // Wait for balance on Alice's account
    aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, AssetQuantity)
    matcherNode.assertAssetBalance(matcherNode.address, aliceAsset, 0)
    bobNode.assertAssetBalance(bobNode.address, aliceAsset, 0)

    //System.currentTimeMillis() - 100000000

    "check correct order filling" in {
      // Alice places sell order
      val currentDateOrderPrepare =
        matcherNode.prepareOrder(aliceNode, aliceWavesPair, OrderType.SELL, price, sellAmount)

      val oldOrderPrepare =
        matcherNode.prepareOrder(aliceNode, aliceWavesPair, OrderType.SELL, price, sellAmount, System.currentTimeMillis() - 100000000)

      val futureDateOrderPrepare =
        matcherNode.prepareOrder(aliceNode, aliceWavesPair, OrderType.SELL, price, sellAmount, System.currentTimeMillis() + 100000000)

      val currentDateOrder = matcherNode.placeOrder(currentDateOrderPrepare)
      val oldOrder         = matcherNode.placeOrder(oldOrderPrepare)
      val futureOrder      = matcherNode.placeOrder(futureDateOrderPrepare)
      currentDateOrder.status shouldBe "OrderAccepted"
      oldOrder.status shouldBe "OrderAccepted"
      futureOrder.status shouldBe "OrderAccepted"

      val orderBuy =
        matcherNode.placeOrder(bobNode, aliceWavesPair, OrderType.BUY, price, sellAmount / 2)

      matcherNode.waitOrderStatus(aliceWavesPair, orderBuy.message.id, "Filled")
      matcherNode.orderStatus(currentDateOrder.message.id, aliceWavesPair).status shouldBe "PartiallyFilled"

      val orderBuy2 =
        matcherNode.placeOrder(bobNode, aliceWavesPair, OrderType.BUY, price, sellAmount)
      matcherNode.waitOrderStatus(aliceWavesPair, orderBuy.message.id, "Filled")
      matcherNode.orderStatus(currentDateOrder.message.id, aliceWavesPair).status shouldBe "Filled"
      matcherNode.orderStatus(oldOrder.message.id, aliceWavesPair).status shouldBe "PartiallyFilled"
      matcherNode.orderStatus(futureOrder.message.id, aliceWavesPair).status shouldBe "Accepted,"

    }
  }
}

object MatcherOrdersOrderTestSuite {

  import ConfigFactory._
  import com.wavesplatform.it.NodeConfigs._

  private val ForbiddenAssetId = "FdbnAsset"
  private val AssetQuantity    = 1000
  private val MatcherFee       = 300000
  private val TransactionFee   = 300000
  private val orderLimit       = 20

  private val minerDisabled = parseString("waves.miner.enable = no")
  private val matcherConfig = parseString(s"""
       |waves.matcher {
       |  enable = yes
       |  account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
       |  bind-address = "0.0.0.0"
       |  order-match-tx-fee = 300000
       |  blacklisted-assets = ["$ForbiddenAssetId"]
       |  balance-watching.enable = yes
       |  rest-order-limit=$orderLimit
       |}""".stripMargin)

  private val Configs: Seq[Config] = (Default.last +: Random.shuffle(Default.init).take(3))
    .zip(Seq(matcherConfig, minerDisabled, minerDisabled, empty()))
    .map { case (n, o) => o.withFallback(n) }
}
