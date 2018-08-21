package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.matcher.configs.MatcherPriceAssetConfig._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._

class CancelOrderTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  matcherNode.signedIssue(createSignedIssueRequest(IssueWctTx))
  nodes.waitForHeightArise()

  "batch cancel" - {

    val ordersNum = 5

    def setOrders(n: Int, pair: AssetPair, orderType: OrderType): Seq[String] = 0 until n map { i =>
      val o =
        matcherNode.placeOrder(matcherNode.prepareOrder(aliceNode, pair, orderType, 1.waves * Order.PriceConstant, 100, (1 + (i & 1)).toByte))
      o.status should be("OrderAccepted")
      o.message.id
    }

    "canceling an order doesn't affect other orders for the same pair" ignore {
      val orders                          = setOrders(ordersNum, wctWavesPair, BUY)
      val (orderToCancel, ordersToRetain) = (orders.head, orders.tail)

      val cancel = matcherNode.cancelOrder(aliceNode, wctWavesPair, Some(orderToCancel))
      cancel.status should be("OrderCanceled")

      ordersToRetain foreach {
        matcherNode.orderStatus(_, wctWavesPair).status should be("Accepted")
      }
    }

    "cancel orders by pair" ignore {
      val ordersToCancel = setOrders(orderLimit + ordersNum, wctWavesPair, BUY)
      val ordersToRetain = setOrders(ordersNum, wavesUsdPair, SELL)
      val ts             = Some(System.currentTimeMillis)

      val cancel = matcherNode.cancelOrder(aliceNode, wctWavesPair, None, ts)
      cancel.status should be("Cancelled")

      ordersToCancel foreach {
        matcherNode.orderStatus(_, wctWavesPair).status should be("Cancelled")
      }
      ordersToRetain foreach {
        matcherNode.orderStatus(_, wavesUsdPair).status should be("Accepted")
      }

      // signed timestamp is mandatory
      assertBadRequestAndMessage(matcherNode.cancelOrder(aliceNode, wctWavesPair, None, None), "invalid signature")

      // timestamp reuse shouldn't be allowed
      //assertBadRequest(matcherNode.cancelOrder(aliceNode, wctWavesPair, None, ts))
    }

    "cancel all orders" ignore {
      val orders1 = setOrders(orderLimit + ordersNum, wctWavesPair, BUY)
      val orders2 = setOrders(orderLimit + ordersNum, wavesUsdPair, SELL)
      val ts      = Some(System.currentTimeMillis)

      val cancel = matcherNode.cancelAllOrders(aliceNode, ts)
      cancel.status should be("Cancelled")

      orders1 foreach {
        matcherNode.orderStatus(_, wctWavesPair).status should be("Cancelled")
      }
      orders2 foreach {
        matcherNode.orderStatus(_, wavesUsdPair).status should be("Cancelled")
      }

      // signed timestamp is mandatory
      assertBadRequestAndMessage(matcherNode.cancelAllOrders(aliceNode, None), "invalid signature")

      // timestamp reuse shouldn't be allowed
      // assertBadRequest(matcherNode.cancelAllOrders(aliceNode, ts))
    }
  }

  "cancel order using api-key" in {
    val orderId = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 800, 100.waves).message.id
    matcherNode.waitOrderStatus(wavesUsdPair, orderId, "Accepted", 1.minute)

    matcherNode.cancelOrderWithApiKey(orderId)
    matcherNode.waitOrderStatus(wavesUsdPair, orderId, "Cancelled", 1.minute)

    matcherNode.fullOrderHistory(bobNode).filter(_.id == orderId).head.status shouldBe "Cancelled"
    matcherNode.orderHistoryByPair(bobNode, wavesUsdPair).filter(_.id == orderId).head.status shouldBe "Cancelled"
    matcherNode.orderBook(wavesUsdPair).bids shouldBe empty
    matcherNode.orderBook(wavesUsdPair).asks shouldBe empty

  }

  "submited order should be canceled" in {
    // Alice wants to sell USD for Waves
    val orderId1      = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 800, 100.waves).message.id
    val orderId2      = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 700, 100.waves).message.id
    val bobSellOrder3 = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 600, 100.waves).message.id

    matcherNode.fullOrderHistory(aliceNode)
    matcherNode.fullOrderHistory(bobNode)

    matcherNode.waitOrderStatus(wavesUsdPair, bobSellOrder3, "Accepted", 1.minute)

    val aliceOrder = matcherNode.placeOrder(aliceNode, wavesUsdPair, OrderType.BUY, 800, 0.00125.waves).message.id
    matcherNode.waitOrderStatus(wavesUsdPair, aliceOrder, "Accepted", 1.minute)

    Thread.sleep(2000)
    matcherNode.fullOrderHistory(aliceNode)
    val orders = matcherNode.fullOrderHistory(bobNode)
    for (orderId <- Seq(orderId1, orderId2)) {
      orders.filter(_.id == orderId).head.status shouldBe "Accepted"
    }
  }

}
