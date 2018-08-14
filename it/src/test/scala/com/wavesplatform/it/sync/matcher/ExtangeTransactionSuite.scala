package com.wavesplatform.it.sync.matcher

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import play.api.libs.json._
import scorex.account.PrivateKeyAccount
import scorex.api.http.assets.SignedExchangeRequest
import scorex.transaction.assets.IssueTransactionV1
import scorex.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import scorex.utils.NTP

class ExchangeTransactionSuite extends BaseTransactionSuite {

  implicit val ord: Writes[Order] = Writes[Order](_.json())

  implicit val exT: OWrites[SignedExchangeRequest] =
    Json.writes[SignedExchangeRequest].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(ExchangeTransaction.typeId.toInt)))

  //  test("exchange tx with expired order in blockchain") {
  //
  //    val assetName        = "myasset"
  //    val assetDescription = "my asset description"
  //
  //    val assetId = sender.issue(firstAddress, assetName, assetDescription, someAssetAmount, 8, false, issueFee).id
  //    nodes.waitForHeightAriseAndTxPresent(assetId)
  //
  //    val buyer                    = pkByAddress(firstAddress)
  //    val seller                   = pkByAddress(firstAddress)
  //    val matcher                  = pkByAddress(thirdAddress)
  //    val time                     = NTP.correctedTime()
  //    val expirationTimestamp      = time + Order.MaxLiveTime
  //    val shortOrderExpirationTime = time + 65000L
  //    val buyPrice                 = 2 * Order.PriceConstant
  //    val sellPrice                = 2 * Order.PriceConstant
  //    val buyAmount                = 1
  //    val sellAmount               = 1
  //    val assetPair                = AssetPair.createAssetPair("WAVES", assetId).get
  //    val buy                      = Order.buy(buyer, matcher, assetPair, buyPrice, buyAmount, time, expirationTimestamp, matcherFee)
  //    val sell                     = Order.sell(seller, matcher, assetPair, sellPrice, sellAmount, time, shortOrderExpirationTime, matcherFee)
  //
  //    val amount = 1
  //    val tx = ExchangeTransaction
  //      .create(
  //        matcher = matcher,
  //        buyOrder = buy,
  //        sellOrder = sell,
  //        price = sellPrice,
  //        amount = amount,
  //        buyMatcherFee = (BigInt(matcherFee) * amount / buy.amount).toLong,
  //        sellMatcherFee = (BigInt(matcherFee) * amount / sell.amount).toLong,
  //        fee = matcherFee,
  //        timestamp = NTP.correctedTime()
  //      )
  //      .right
  //      .get
  //
  //    implicit val o = Writes[Order](_.json())
  //
  //    implicit val w =
  //      Json.writes[SignedExchangeRequest].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(ExchangeTransaction.typeId.toInt)))
  //
  //    def request(tx: ExchangeTransaction): SignedExchangeRequest =
  //      SignedExchangeRequest(
  //        Base58.encode(tx.sender.publicKey),
  //        tx.buyOrder,
  //        tx.sellOrder,
  //        tx.price,
  //        tx.amount,
  //        matcherFee,
  //        tx.buyMatcherFee,
  //        tx.sellMatcherFee,
  //        tx.timestamp,
  //        tx.signature.base58
  //      )
  //
  //    log.info(s"${NTP.correctedTime()}")
  //    log.info(s"$shortOrderExpirationTime")
  //    log.info(s"${shortOrderExpirationTime - (NTP.correctedTime() - time) - time - 500}")
  //    Thread.sleep(shortOrderExpirationTime - (NTP.correctedTime() - time) - time - 500)
  //    sender.broadcastRequest(request(tx))
  //    nodes.waitForHeightAriseAndTxPresent(tx.id().base58)
  //  }

  test("exchange tx with not asset not exists") {

    val assetName        = "myasset"
    val assetDescription = "my asset description"

    val IssueTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = sender.privateKey,
        name = assetName.getBytes(),
        description = assetDescription.getBytes(),
        quantity = someAssetAmount,
        decimals = 8,
        reissuable = true,
        fee = 1.waves,
        timestamp = System.currentTimeMillis()
      )
      .right
      .get

    val assetId = IssueTx.id().base58

    val buyer                      = pkByAddress(firstAddress)
    val seller                     = pkByAddress(firstAddress)
    val matcher: PrivateKeyAccount = pkByAddress(thirdAddress)
    val time                       = NTP.correctedTime()
    val expirationTimestamp        = time + Order.MaxLiveTime
    val buyPrice                   = 2 * Order.PriceConstant
    val sellPrice                  = 2 * Order.PriceConstant
    val shortOrderExpirationTime   = time + 65000L
    val buyAmount                  = 1
    val sellAmount                 = 1
    val assetPair                  = AssetPair.createAssetPair("WAVES", assetId).get
    val buy                        = Order.buy(buyer, matcher, assetPair, buyPrice, buyAmount, time, expirationTimestamp, matcherFee)
    val sell                       = Order.sell(seller, matcher, assetPair, sellPrice, sellAmount, time, shortOrderExpirationTime, matcherFee)

    val amount = 1

    val (txId, signedTx) = signExchangeTx(matcher, buy, sell, sellPrice, amount, time + 1)
    sender.broadcastRequest(signedTx)

    // nodes.waitForHeightAriseAndTxPresent(txId)
  }

}
