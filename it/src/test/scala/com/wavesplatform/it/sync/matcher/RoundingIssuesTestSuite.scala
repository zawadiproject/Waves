package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.matcher.configs.MatcherPriceAssetConfig._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.transaction.assets.exchange.OrderType
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._

class RoundingIssuesTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  nodes.waitForHeightArise()

  "should correctly fill an order with small amount" in {
    val aliceBalanceBefore = matcherNode.accountBalances(aliceNode.address)._1
    val bobBalanceBefore   = matcherNode.accountBalances(bobNode.address)._1

    val order1   = matcherNode.prepareOrder(aliceNode, wavesUsdPair, OrderType.BUY, 238, 3100000000L)
    val order1Id = matcherNode.placeOrder(order1).message.id

    val order2   = matcherNode.prepareOrder(bobNode, wavesUsdPair, OrderType.SELL, 235, 425532L)
    val order2Id = matcherNode.placeOrder(order2).message.id

    matcherNode.waitOrderStatus(wavesUsdPair, order2Id, "Filled", 1.minute)
    matcherNode.waitOrderStatus(wavesUsdPair, order1Id, "PartiallyFilled", 1.minute)

    matcherNode.cancelOrder(aliceNode, wavesUsdPair, Some(order1Id))
    val tx = matcherNode.transactionsByOrder(order1Id).head

    matcherNode.waitForTransaction(tx.id)
    val rawExchangeTx = matcherNode.rawTransactionInfo(tx.id)

    (rawExchangeTx \ "price").as[Long] shouldBe 238L
    (rawExchangeTx \ "amount").as[Long] shouldBe 420169L
    (rawExchangeTx \ "buyMatcherFee").as[Long] shouldBe 40L
    (rawExchangeTx \ "sellMatcherFee").as[Long] shouldBe 296219L

    val aliceBalanceAfter = matcherNode.accountBalances(aliceNode.address)._1
    val bobBalanceAfter   = matcherNode.accountBalances(bobNode.address)._1

    (aliceBalanceAfter - aliceBalanceBefore) shouldBe (-40L + 420169L)
    (bobBalanceAfter - bobBalanceBefore) shouldBe (-296219L - 420169L)
  }

}
