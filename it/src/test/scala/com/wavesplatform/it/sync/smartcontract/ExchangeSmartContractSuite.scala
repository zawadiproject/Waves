package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.api.http.assets.SignedExchangeRequest
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.matcher.configs.MatcherPriceAssetConfig._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state._
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.v1.ScriptV1
import com.wavesplatform.utils.{NTP, dummyCompilerContext}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import play.api.libs.json.{JsNumber, OWrites, Writes, _}

import scala.concurrent.Await
import com.wavesplatform.it.util._
/*
Scenario:
every month a foundation makes payments from two MassTransactions(type == 11):
1) 80% to users
2) 10% as tax and 10% to bank go after 30sec of payment from step 1)
 */

class ExchangeSmartContractSuite
    extends FreeSpec
    with NodesFromDocker
    with ReportingTestName
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def buyer                 = nodes(1)
  private def seller                = nodes(2)

  implicit val ord: Writes[Order] = Writes[Order](_.json())
  implicit val exT: OWrites[SignedExchangeRequest] =
    Json.writes[SignedExchangeRequest].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(ExchangeTransaction.typeId.toInt)))

  matcherNode.signedIssue(createSignedIssueRequest(IssueWctTx))
  nodes.waitForHeightArise()

  "exchange transaction suite" in {

    val scriptText = {
      val untyped = Parser(s"""
                              |match tx {
                              | case ttx: ExchangeTransaction =>
                              |    let ePrice = (ttx.price)
                              |    let buyPrice = (ttx.buyOrder.price)
                              |    let sellPrice = (ttx.sellOrder.price)
                              |    ((ePrice == buyPrice) && (buyPrice != sellPrice))
                              | case other => false
                              |
           | }
        """.stripMargin).get.value
      assert(untyped.size == 1)
      CompilerV1(dummyCompilerContext, untyped.head).explicitGet()._1
    }

    // set script
    val script = ScriptV1(scriptText).explicitGet()
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(SetScriptTransaction.supportedVersions.head, matcherNode.privateKey, Some(script), minFee, System.currentTimeMillis())
      .explicitGet()

    val setScriptId = matcherNode
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(matcherNode.address)

    matcherNode.addressScriptInfo(matcherNode.address).scriptText.isEmpty shouldBe false

    val time                     = NTP.correctedTime()
    val expirationTimestamp      = time + Order.MaxLiveTime
    val buyPrice                 = 26 * Order.PriceConstant
    val sellPrice                = 22 * Order.PriceConstant
    val shortOrderExpirationTime = time + 65000L
    val buyAmount                = 1
    val sellAmount               = 1
    val buy                      = Order.buy(buyer.privateKey, matcherNode.publicKey, wctWavesPair, buyPrice, buyAmount, time, expirationTimestamp, matcherFee)
    val sell                     = Order.sell(seller.privateKey, matcherNode.publicKey, wctWavesPair, sellPrice, sellAmount, time, shortOrderExpirationTime, matcherFee)
    val amount                   = 1
    val (txId, signedTx)         = signExchangeTx(matcherNode.privateKey, buy, sell, buyPrice, amount, time + 1)
    matcherNode.broadcastRequest(signedTx)

    nodes.waitForHeightAriseAndTxPresent(txId)
  }

//  "exchange transaction suite fail" in {
//
//    val scriptText = {
//      val untyped = Parser(s"""
//           |match tx {
//           | case ttx: ExchangeTransaction =>
//           |    let ePrice = (ttx.price)
//           |    let buyPrice = (ttx.buyOrder.price)
//           |    let sellPrice = (ttx.sellOrder.price)
//           |    ((ePrice == buyPrice) && (buyPrice != sellPrice))
//           | case other => false
//           |
//           | }
//        """.stripMargin).get.value
//      assert(untyped.size == 1)
//      CompilerV1(dummyCompilerContext, untyped.head).explicitGet()._1
//    }
//
//    // set script
//    val script = ScriptV1(scriptText).explicitGet()
//    val setScriptTransaction = SetScriptTransaction
//      .selfSigned(SetScriptTransaction.supportedVersions.head, sender.privateKey, Some(script), minFee, System.currentTimeMillis())
//      .explicitGet()
//
//    val setScriptId = sender
//      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
//      .id
//
//    nodes.waitForHeightAriseAndTxPresent(setScriptId)
//
//    sender.addressScriptInfo(sender.address).scriptText.isEmpty shouldBe false
//
//    val time                     = NTP.correctedTime()
//    val expirationTimestamp      = time + Order.MaxLiveTime
//    val buyPrice                 = 2 * Order.PriceConstant
//    val sellPrice                = 2 * Order.PriceConstant
//    val shortOrderExpirationTime = time + 65000L
//    val buyAmount                = 1
//    val sellAmount               = 1
//    val buy                      = Order.buy(buyer.privateKey, matcherNode.publicKey, wctWavesPair, buyPrice, buyAmount, time, expirationTimestamp, matcherFee)
//    val sell                     = Order.sell(seller.privateKey, matcherNode.publicKey, wctWavesPair, sellPrice, sellAmount, time, shortOrderExpirationTime, matcherFee)
//    val amount                   = 1
//    val (txId, signedTx)         = signExchangeTx(matcherNode.privateKey, buy, sell, sellPrice, amount, time + 1)
//    sender.broadcastRequest(signedTx)
//
//    nodes.waitForHeightAriseAndTxPresent(txId)
//  }

  //save time
  //    val currTime = NTP.correctedTime()
  //
  //
  //    val unsignedBuyOrder: Order = Order(bobNode.publicKey, matcherNode.publicKey, wctWavesPair, OrderType.SELL, 3000000000000l, 200, currTime, currTime + Order.MaxLiveTime, matcherFee, Array())
  //
  //    val unsignedSellOrder = Order(aliceNode.publicKey, matcherNode.publicKey, wctWavesPair, OrderType.BUY, 3000000000000l, 180, currTime+1, currTime + Order.MaxLiveTime, matcherFee, Array())
  //
  //
  //    val aliceSig = ByteStr(crypto.sign(bobNode.privateKey, unsignedBuyOrder.bodyBytes()))
  //    val signedBuyOrder = unsignedBuyOrder.copy(proofs = Proofs(Seq(aliceSig))
  //
  ////    val exchange =
  ////      ExchangeTransaction()
  //
  //    val unsigned =
  //      MassTransferTransaction
  //        .create(1, None, aliceNode.publicKey, transfers, currTime, calcMassTransferFee(2) + smartFee, Array.emptyByteArray, Proofs.empty)
  //        .explicitGet()
  //
  //    val accountSig = ByteStr(crypto.sign(aliceNode.privateKey, unsigned.bodyBytes()))
  //    val signed     = unsigned.copy(proofs = Proofs(Seq(aliceSig)))
  //    val toUsersID  = aliceNode.signedBroadcast(signed.json() + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt))).id
  //
  //    nodes.waitForHeightAriseAndTxPresent(toUsersID)
  //
  //    //make transfer with incorrect time
  //    val heightBefore = aliceNode.height
  //
  //    val transfersToGov =
  //      MassTransferTransaction.parseTransfersList(List(Transfer(firstAddress, transferAmount), Transfer(fourthAddress, transferAmount))).explicitGet()
  //
  //    val unsignedToGov =
  //      MassTransferTransaction
  //        .create(1, None, aliceNode.publicKey, transfersToGov, currTime, calcMassTransferFee(2) + smartFee, Array.emptyByteArray, Proofs.empty)
  //        .explicitGet()
  //    val accountSigToGovFail = ByteStr(crypto.sign(aliceNode.privateKey, unsignedToGov.bodyBytes()))
  //    val signedToGovFail     = unsignedToGov.copy(proofs = Proofs(Seq(accountSigToGovFail)))
  //
  //    assertBadRequestAndResponse(
  //      aliceNode.signedBroadcast(signedToGovFail.json() + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt))),
  //      "Transaction not allowed by account-script"
  //    )
  //
  //    //make correct transfer to government after some time
  //    aliceNode.waitForHeight(heightBefore + 10, 2.minutes)
  //
  //    val unsignedToGovSecond =
  //      MassTransferTransaction
  //        .create(1,
  //                None,
  //                aliceNode.publicKey,
  //                transfersToGov,
  //                System.currentTimeMillis(),
  //                calcMassTransferFee(2) + smartFee,
  //                Array.emptyByteArray,
  //                Proofs.empty)
  //        .explicitGet()
  //
  //    val accountSigToGov = ByteStr(crypto.sign(aliceNode.privateKey, unsignedToGovSecond.bodyBytes()))
  //    val signedToGovGood = unsignedToGovSecond.copy(proofs = Proofs(Seq(accountSigToGov, ByteStr(Base58.decode(toUsersID).get))))
  //    val massTransferID  = aliceNode.signedBroadcast(signedToGovGood.json() + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt))).id
  //
  //    nodes.waitForHeightAriseAndTxPresent(massTransferID)
//  }
}
