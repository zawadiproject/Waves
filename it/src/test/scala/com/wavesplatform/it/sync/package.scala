package com.wavesplatform.it

import com.wavesplatform.state.DataEntry
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.smart.script.ScriptCompiler

package object sync {
  val smartExtraFee: Long              = 0.004.waves
  val minFee: Long                     = 0.001.waves
  val leasingFee: Long                 = 0.002.waves
  val issueFee: Long                   = 1.waves
  val burnFee: Long                    = 1.waves
  val sponsorFee: Long                 = 1.waves
  val setAssetScriptFee: Long          = 1.waves
  val setScriptFee: Long               = 0.01.waves
  val transferAmount: Long             = 10.waves
  val leasingAmount: Long              = transferAmount
  val issueAmount: Long                = transferAmount
  val massTransferFeePerTransfer: Long = 0.0005.waves
  val someAssetAmount: Long            = 9999999999999l
  val minMatcherFee: Long              = 0.003.waves
  val smartMatcherFee: Long            = 0.007.waves
  val smartMinFee: Long                = minFee + smartExtraFee
  val fullMatcherFee: Long             = 0.015.waves

  def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      minFee * (dataSize / 1024 + 1)
    } else minFee
  }

  def calcMassTransferFee(numberOfRecipients: Int): Long = {
    minFee + massTransferFeePerTransfer * (numberOfRecipients + 1)
  }

  val supportedVersions = List(null, "2") //sign and broadcast use default for V1

  val script       = ScriptCompiler(s"""true""".stripMargin).explicitGet()._1
  val scriptBase64 = script.bytes.value.base64

  val errNotAllowedByToken = "Transaction is not allowed by token-script"
}
