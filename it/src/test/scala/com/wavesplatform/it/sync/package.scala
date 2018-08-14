package com.wavesplatform.it

import com.wavesplatform.it.util._
import com.wavesplatform.state.DataEntry
import com.wavesplatform.utils.Base58
import scorex.account.PrivateKeyAccount
import scorex.api.http.assets.SignedExchangeRequest
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}

package object sync {
  val fee                        = 0.001.waves
  val leasingFee                 = 0.002.waves
  val smartFee                   = 0.004.waves
  val issueFee                   = 1.waves
  val transferAmount             = 10.waves
  val leasingAmount              = transferAmount
  val issueAmount                = transferAmount
  val massTransferFeePerTransfer = 0.0005.waves
  val someAssetAmount            = 100000
  val matcherFee                 = 0.003.waves

  def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      fee * (dataSize / 1024 + 1)
    } else fee
  }

  def calcMassTransferFee(numberOfRecipients: Int): Long = {
    fee + massTransferFeePerTransfer * (numberOfRecipients + 1)
  }

  val supportedVersions = List(null, "2") //sign and broadcast use default for V1

  def signExchangeTx(matcher: PrivateKeyAccount,
                     buyOrder: Order,
                     sellOrder: Order,
                     price: Long,
                     amount: Long,
                     timestamp: Long,
                     buyMatcherFee: Long = 300000,
                     sellMatcherFee: Long = 300000,
                     fee: Long = 300000): (String, SignedExchangeRequest) = {
    val tx = ExchangeTransaction
      .create(
        matcher = matcher,
        buyOrder = buyOrder,
        sellOrder = sellOrder,
        price = price,
        amount = amount,
        buyMatcherFee = (BigInt(matcherFee) * amount / buyOrder.amount).toLong,
        sellMatcherFee = (BigInt(matcherFee) * amount / sellOrder.amount).toLong,
        fee = matcherFee,
        timestamp = timestamp
      )
      .right
      .get

    val txId: String = tx.id().base58

    def request(tx: ExchangeTransaction): SignedExchangeRequest =
      SignedExchangeRequest(
        Base58.encode(tx.sender.publicKey),
        tx.buyOrder,
        tx.sellOrder,
        tx.price,
        tx.amount,
        tx.fee,
        tx.buyMatcherFee,
        tx.sellMatcherFee,
        tx.timestamp,
        tx.signature.base58
      )

    val exchangeRequest: SignedExchangeRequest = request(tx)

    (txId, exchangeRequest)
  }
}
