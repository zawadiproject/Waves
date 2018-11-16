package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, _}
import com.wavesplatform.utils.NTP
import org.scalatest.CancelAfterFailure
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.DataTransaction
import play.api.libs.json._
import scorex.crypto.encode.Base64

class ExchangeTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)

  var exchAsset: String    = ""
  var dtx: DataTransaction = null
  var assetPair: AssetPair = null

  val sc1 = Some(s"""true""")
  val sc2 = Some(s"""
               |match tx {
               |  case s : SetScriptTransaction => true
               |  case _ => false
               |}""".stripMargin)
  val sc3 = Some(s"""
               |match tx {
               |  case s : SetScriptTransaction => true
               |  case _ => throw("Some generic error")
               |}""".stripMargin)

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    exchAsset = sender
      .issue(acc0.address, "ExchangeCoin", "ExchangeCoin for tests with exchange transaction", someAssetAmount, 0, reissuable = false, issueFee, 2)
      .id
    nodes.waitForHeightAriseAndTxPresent(exchAsset)
    assetPair = AssetPair.createAssetPair(exchAsset, "WAVES").get

    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", true)
    val entry3 = BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=")))
    val entry4 = StringDataEntry("str", "test")

    dtx = DataTransaction.selfSigned(1, acc0, List(entry1, entry2, entry3, entry4), 0.001.waves, NTP.correctedTime()).explicitGet()
    val dtxId = sender.signedBroadcast(dtx.json()).id
    nodes.waitForHeightAriseAndTxPresent(dtxId)
  }

  test("set contracts and put exchange transaction in blockchain") {
    val sc4 = Some(cryptoContextScript)
    val sc5 = Some(pureContextScript(dtx))
    val sc6 = Some(wavesContextScript(dtx))

    /* combinations of accounts and fees - orders + extx
    minMatcherFee - 0.003.waves
    smartMatcherFee - 0.007.waves
    fullMatcherFee - 0.015.waves
     * */
    for ((contr1, contr2, mcontr, buyfee, sellfee, mfee) <- Seq(
           (sc1, sc1, sc1, smartMatcherFee, smartMatcherFee, fullMatcherFee),
           (None, sc1, None, minMatcherFee, smartMatcherFee, smartMatcherFee),
           (None, None, sc1, minMatcherFee, minMatcherFee, smartMatcherFee),
           (None, None, sc4, minMatcherFee, minMatcherFee, smartMatcherFee),
           (None, None, sc5, minMatcherFee, minMatcherFee, smartMatcherFee),
           (None, None, sc6, minMatcherFee, minMatcherFee, smartMatcherFee)
         )) {
      setContract(contr1, acc0)
      setContract(contr2, acc1)
      setContract(mcontr, acc2)

      val tx = exchangeTx(acc1, acc0, acc2, assetPair, buyfee, sellfee, mfee)

      val txId = sender.signedBroadcast(tx).id
      nodes.waitForHeightAriseAndTxPresent(txId)
      //TODO : add assert balances
    }
    setContract(None, acc0)
    setContract(None, acc1)
    setContract(None, acc2)
  }

  test("negative: set simple contracts and put exchange transaction in blockchain") {
    /* combinations of accounts and fees - orders + extx
    minMatcherFee - 0.003.waves
    smartMatcherFee - 0.007.waves
    fullMatcherFee - 0.015.waves
     * */
    for ((contr1, contr2, mcontr, buyfee, sellfee, mfee) <- Seq(
           (sc1, sc2, sc1, smartMatcherFee, smartMatcherFee, fullMatcherFee),
           (sc1, sc1, sc2, smartMatcherFee, smartMatcherFee, fullMatcherFee),
           (None, None, sc2, minMatcherFee, minMatcherFee, smartMatcherFee),
           (None, sc2, None, minMatcherFee, smartMatcherFee, smartMatcherFee)
         )) {
      setContract(contr1, acc0)
      setContract(contr2, acc1)
      setContract(mcontr, acc2)

      val tx = exchangeTx(acc1, acc0, acc2, assetPair, buyfee, sellfee, mfee)
      assertBadRequestAndMessage(sender.signedBroadcast(tx), "Transaction is not allowed by account-script")
      //TODO : add assert balances
    }
    setContract(None, acc0)
    setContract(None, acc1)
    setContract(None, acc2)
  }

  test("negative: check custom exception") {
    /* combinations of accounts and fees - orders + extx
    minMatcherFee - 0.003.waves
    smartMatcherFee - 0.007.waves
    fullMatcherFee - 0.015.waves
     * */
    for ((contr1, contr2, mcontr, buyfee, sellfee, mfee) <- Seq(
           (sc1, sc1, sc3, smartMatcherFee, smartMatcherFee, fullMatcherFee)
         )) {
      setContract(contr1, acc0)
      setContract(contr2, acc1)
      setContract(mcontr, acc2)

      val tx = exchangeTx(acc1, acc0, acc2, assetPair, buyfee, sellfee, mfee)
      assertBadRequestAndMessage(sender.signedBroadcast(tx), "Error while executing account-script: Some generic error")
      //TODO : add assert balances
    }
    setContract(None, acc0)
    setContract(None, acc1)
    setContract(None, acc2)
  }

  test("positive: versioning verification") {
    /* combinations of accounts and fees - orders + extx
    minMatcherFee - 0.003.waves
    smartMatcherFee - 0.007.waves
    fullMatcherFee - 0.015.waves
     * */
    for ((contr1, contr2, mcontr, buyfee, sellfee, mfee) <- Seq(
//           (None, None, None, minMatcherFee, minMatcherFee, minMatcherFee) //,
           (sc1, None, None, smartMatcherFee, minMatcherFee, smartMatcherFee),
           (None, None, sc1, minMatcherFee, minMatcherFee, smartMatcherFee)
         )) {
      setContract(contr1, acc0)
      setContract(contr2, acc1)
      setContract(mcontr, acc2)

      val matcher   = acc2
      val sellPrice = (0.50 * Order.PriceConstant).toLong
      val buy       = orders(1, acc1, acc0, acc2, assetPair)._1
      val sell      = orders(2, acc1, acc0, acc2, assetPair)._2

      val amount = math.min(buy.amount, sell.amount)
      val tx = ExchangeTransactionV2
        .create(
          matcher = matcher,
          buyOrder = buy,
          sellOrder = sell,
          amount = amount,
          price = sellPrice,
          buyMatcherFee = buyfee,
          sellMatcherFee = sellfee,
          fee = mfee,
          timestamp = NTP.correctedTime()
        )
        .explicitGet()
        .json()

      val txId = sender.signedBroadcast(tx).id
      nodes.waitForHeightAriseAndTxPresent(txId)

      //TODO : add assert balances
    }
    setContract(None, acc0)
    setContract(None, acc1)
    setContract(None, acc2)
  }

  test("negative: check orders v2 with exchange tx v1") {
    val tx        = exchangeTx(acc1, acc0, acc2, assetPair)
    val sig       = (Json.parse(tx.toString()) \ "proofs").as[Seq[JsString]].head
    val changedTx = tx + ("version" -> JsNumber(1)) + ("signature" -> sig)
    assertBadRequestAndMessage(sender.signedBroadcast(changedTx).id, "can only contain orders of version 1", 400)
  }

  test("negative: exchange tx v2 and order v1 from scripted acc") {
    setContract(sc1, acc0)

    val mf        = smartMatcherFee
    val matcher   = acc2
    val sellPrice = (0.50 * Order.PriceConstant).toLong
    val buy       = orders(version = 2, acc1, acc0, acc2, assetPair)._1
    val sell      = orders(version = 1, acc1, acc0, acc2, assetPair)._2

    val amount = math.min(buy.amount, sell.amount)
    val tx = ExchangeTransactionV2
      .create(
        matcher = matcher,
        buyOrder = buy,
        sellOrder = sell,
        amount = amount,
        price = sellPrice,
        buyMatcherFee = smartMatcherFee,
        sellMatcherFee = minMatcherFee,
        fee = mf,
        timestamp = NTP.correctedTime()
      )
      .explicitGet()
      .json()

    assertBadRequestAndMessage(sender.signedBroadcast(tx).id, "Reason: Can't process order with signature from scripted account")
  }

}
