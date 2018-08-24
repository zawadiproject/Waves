package com.wavesplatform.it.sync.matcher

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.config.ConfigFactory.{empty, parseString}
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.api.http.assets.SignedIssueV1Request
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.AssetDecimalsInfo
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.CustomFeeTransactionSuite.defaultAssetQuantity
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.utils.Base58
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode
import scala.util.Random

class MatcherTickersTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {

  import MatcherTickersTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))

  "validate mathcer orderbook statuses" - {
    "get tickers for unavaliable asset should produce error" in {
      //      assertNotFoundAndMessage(matcherNode.marketStatus(wctWavesPair), s"Invalid Asset ID: ${IssueWctTx.id()}")
    }

    "issue tokens" in {
      matcherNode.signedIssue(createSignedIssueRequest(IssueWctTx))
      nodes.waitForHeightArise()

    }

    "status for empty orderook" in {
      //      assertNotFoundAndMessage(matcherNode.marketStatus(wctWavesPair), s"Invalid Asset ID: ${IssueWctTx.id()}")
    }

    val bidPrice  = 423
    val bidAmount = 1.waves
    val askPrice  = 511
    val askAmount = 0.5.waves

    "place bid order" in {
      matcherNode.placeOrder(aliceNode, wavesUsdPair, OrderType.BUY, bidPrice, bidAmount)
      val aliceOrder = matcherNode.placeOrder(aliceNode, wavesUsdPair, OrderType.BUY, bidPrice, bidAmount).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, aliceOrder, "Accepted")
      val r = matcherNode.marketStatus(wavesUsdPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(bidAmount)
      r.ask shouldBe None
      r.askAmount shouldBe None
    }

    "place bid order" in {
      matcherNode.placeOrder(aliceNode, wavesUsdPair, OrderType.BUY, askPrice, askAmount)
      val aliceOrder = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, askPrice, askAmount).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, aliceOrder, "Accepted")
      val r = matcherNode.marketStatus(wavesUsdPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount)
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(askAmount * 2)
    }
  }

  "Alice and Bob trade WAVES-USD" - {
    nodes.waitForHeightArise()
    val aliceWavesBalanceBefore = matcherNode.accountBalances(aliceNode.address)._1
    val bobWavesBalanceBefore   = matcherNode.accountBalances(bobNode.address)._1
    //
    //    val price = 238
    //    val buyOrderAmount = 425532L
    //    val sellOrderAmount = 3100000000L
    //
    //    val correctedSellAmount = correctAmount(sellOrderAmount, price)
    //
    //    val adjustedAmount = receiveAmount(OrderType.BUY, price, buyOrderAmount)
    //    val adjustedTotal = receiveAmount(OrderType.SELL, price, buyOrderAmount)
    //
    //    log.debug(s"correctedSellAmount: $correctedSellAmount, adjustedAmount: $adjustedAmount, adjustedTotal: $adjustedTotal")
    //
    //    "place usd-waves order" in {
    //      // Alice wants to sell USD for Waves
    //
    //      val bobOrder1 = matcherNode.prepareOrder(bobNode, wavesUsdPair, OrderType.SELL, price, sellOrderAmount)
    //      val bobOrder1Id = matcherNode.placeOrder(bobOrder1).message.id
    //      matcherNode.waitOrderStatus(wavesUsdPair, bobOrder1Id, "Accepted", 1.minute)
    //      matcherNode.reservedBalance(bobNode)("WAVES") shouldBe sellOrderAmount + matcherFee
    //      matcherNode.tradableBalance(bobNode, wavesUsdPair)("WAVES") shouldBe bobWavesBalanceBefore - (sellOrderAmount + matcherFee)
    //
    //      val aliceOrder = matcherNode.prepareOrder(aliceNode, wavesUsdPair, OrderType.BUY, price, buyOrderAmount)
    //      val aliceOrderId = matcherNode.placeOrder(aliceOrder).message.id
    //      matcherNode.waitOrderStatusAndAmount(wavesUsdPair, aliceOrderId, "Filled", Some(420169L), 1.minute)
    //
    //      // Bob wants to buy some USD
    //      matcherNode.waitOrderStatusAndAmount(wavesUsdPair, bobOrder1Id, "PartiallyFilled", Some(420169L), 1.minute)
    //
    //      // Each side get fair amount of assets
    //      val exchangeTx = matcherNode.transactionsByOrder(aliceOrder.idStr()).headOption.getOrElse(fail("Expected an exchange transaction"))
    //      nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)
    //    }

  }

}

object MatcherTickersTestSuite {

  import ConfigFactory._
  import com.wavesplatform.it.NodeConfigs._

  private val ForbiddenAssetId = "FdbnAsset"
  val Decimals: Byte           = 2

  private val minerDisabled = parseString("waves.miner.enable = no")
  private val matcherConfig = parseString(s"""
       |waves.matcher {
       |  enable = yes
       |  account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
       |  bind-address = "0.0.0.0"
       |  order-match-tx-fee = 300000
       |  blacklisted-assets = ["$ForbiddenAssetId"]
       |  balance-watching.enable = yes
       |}""".stripMargin)

  private val _Configs: Seq[Config] = (Default.last +: Random.shuffle(Default.init).take(3))
    .zip(Seq(matcherConfig, minerDisabled, minerDisabled, empty()))
    .map { case (n, o) => o.withFallback(n) }

  private val aliceSeed = _Configs(1).getString("account-seed")
  private val bobSeed   = _Configs(2).getString("account-seed")
  private val alicePk   = PrivateKeyAccount.fromSeed(aliceSeed).right.get
  private val bobPk     = PrivateKeyAccount.fromSeed(bobSeed).right.get

  val usdAssetName = "USD-X"
  val wctAssetName = "WCT-X"
  val IssueUsdTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = alicePk,
      name = usdAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueWctTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = bobPk,
      name = wctAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val UsdId: AssetId = IssueUsdTx.id()
  val WctId          = IssueWctTx.id()

  val wctUsdPair = AssetPair(
    amountAsset = Some(WctId),
    priceAsset = Some(UsdId)
  )

  val wctWavesPair = AssetPair(
    amountAsset = Some(WctId),
    priceAsset = None
  )

  val wavesUsdPair = AssetPair(
    amountAsset = None,
    priceAsset = Some(UsdId)
  )

  private val updatedMatcherConfig = parseString(s"""
       |waves.matcher {
       |  price-assets = [ "$UsdId", "WAVES"]
       |}
     """.stripMargin)

  private val Configs = _Configs.map(updatedMatcherConfig.withFallback(_))

  def createSignedIssueRequest(tx: IssueTransactionV1): SignedIssueV1Request = {
    import tx._
    SignedIssueV1Request(
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      signature.base58
    )
  }
}
