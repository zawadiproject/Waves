package com.wavesplatform.state.diffs

import cats.Monoid
import cats.implicits._
import cats.syntax.either.catsSyntaxEitherId
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.state.patch.{CancelAllLeases, CancelInvalidLeaseIn, CancelLeaseOverflow}
import com.wavesplatform.state.reader.CompositeBlockchain.composite
import com.wavesplatform.account.Address
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.transaction.ValidationError.ActivationError
import com.wavesplatform.transaction.{Transaction, ValidationError}

object BlockDiffer extends ScorexLogging with Instrumented {

  private def clearSponsorship(blockchain: Blockchain, portfolio: Portfolio, height: Int, fs: FunctionalitySettings): Portfolio = {
    if (height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain, fs)) {
      val minerFee = portfolio.assets.map {
        case (assetId, assetFee) =>
          val baseFee = blockchain.assetDescription(assetId).get.sponsorship
          Sponsorship.toWaves(assetFee, baseFee)
      }.sum
      Portfolio.empty.copy(balance = portfolio.balance + minerFee)
    } else portfolio
  }

  def fromBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                blockchain: Blockchain,
                                                maybePrevBlockFees: Option[Portfolio],
                                                maybePrevBlockTimestamp: Option[Long],
                                                block: Block,
                                                constraint: Constraint): Either[ValidationError, (Diff, Portfolio, Constraint)] = {
    // height switch is next after activation
    val ng4060switchHeight = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)

    lazy val currentBlockFeeDistr =
      if (blockchain.height < ng4060switchHeight)
        Some(block.feesPortfolio())
      else
        None

    for {
      _ <- block.signaturesValid()
      r <- apply(
        settings,
        blockchain,
        constraint,
        maybePrevBlockTimestamp,
        block.signerData.generator,
        maybePrevBlockFees,
        currentBlockFeeDistr,
        block.timestamp,
        block.transactionData,
        1
      )
    } yield r
  }

  def fromMicroBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                     blockchain: Blockchain,
                                                     prevBlockTimestamp: Option[Long],
                                                     micro: MicroBlock,
                                                     timestamp: Long,
                                                     constraint: Constraint): Either[ValidationError, (Diff, Portfolio, Constraint)] = {
    for {
      // microblocks are processed within block which is next after 40-only-block which goes on top of activated height
      _ <- Either.cond(blockchain.activatedFeatures.contains(BlockchainFeatures.NG.id), (), ActivationError(s"MicroBlocks are not yet activated"))
      _ <- micro.signaturesValid()
      r <- apply(
        settings,
        blockchain,
        constraint,
        prevBlockTimestamp,
        micro.sender,
        None,
        None,
        timestamp,
        micro.transactionData,
        0
      )
    } yield r
  }

  private def apply[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                    blockchain: Blockchain,
                                                    initConstraint: Constraint,
                                                    prevBlockTimestamp: Option[Long],
                                                    blockGenerator: Address,
                                                    prevBlockFeeDistr: Option[Portfolio],
                                                    currentBlockFeeDistr: Option[Portfolio],
                                                    timestamp: Long,
                                                    txs: Seq[Transaction],
                                                    heightDiff: Int): Either[ValidationError, (Diff, Portfolio, Constraint)] = { ///Option[Portfolio]
    def updateConstraint(constraint: Constraint, blockchain: Blockchain, tx: Transaction): Constraint =
      constraint.put(blockchain, tx).asInstanceOf[Constraint]

    val currentBlockHeight = blockchain.height + heightDiff
    val txDiffer           = TransactionDiffer(settings, prevBlockTimestamp, timestamp, currentBlockHeight) _

    val txsDiffEi = currentBlockFeeDistr match {
      case Some(feeDistr) =>
        val initDiff = Diff.empty.copy(portfolios = Map(blockGenerator -> feeDistr))
        println(s"BD oo h=${blockchain.height}, feeDistr = ${feeDistr.balance} WAVES") ///
        txs
          .foldLeft((initDiff, initConstraint).asRight[ValidationError]) {
            case (r @ Left(_), _) => r
            case (Right((currDiff, currConstraint)), tx) =>
              val updatedBlockchain = composite(blockchain, currDiff)
              val updatedConstraint = updateConstraint(currConstraint, updatedBlockchain, tx)
              if (updatedConstraint.isOverfilled)
                Left(ValidationError.GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint"))
              else
                txDiffer(updatedBlockchain, tx).map { newDiff =>
                  (currDiff.combine(newDiff), updatedConstraint)
                }
          }
          .map { case (diff, constraint) => (diff, Portfolio.empty, constraint) }
      case None =>
        val prevBlockFees = prevBlockFeeDistr.map(p => p.minus(p.multiply(Block.CurrentBlockFeePart))).orEmpty
        println(s"BD NG h=${blockchain.height}, prev block fee = ${prevBlockFees.balance} WAVES") ///
//        val initDiff = Diff.empty.copy(portfolios = Map(blockGenerator -> prevBlockFees))
        txs
          .foldLeft((Diff.empty, Portfolio.empty, initConstraint).asRight[ValidationError]) {
            case (r @ Left(_), _) => r
            case (Right((currDiff, currFees, currConstraint)), tx) =>
              val updatedBlockchain = composite(blockchain, currDiff)
              val updatedConstraint = updateConstraint(currConstraint, updatedBlockchain, tx)
              if (updatedConstraint.isOverfilled)
                Left(ValidationError.GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint"))
              else
                txDiffer(updatedBlockchain, tx).map { newDiff =>
                  val totalBlockFee = clearSponsorship(updatedBlockchain, tx.feeDiff(), currentBlockHeight, settings)
                  (currDiff.combine(newDiff), currFees.combine(totalBlockFee), updatedConstraint)
                }
          }
          .map {
            case (diff, totalFees, constraint) =>
              val minerFees      = totalFees.multiply(Block.CurrentBlockFeePart)
              val totalMinerFees = prevBlockFees.combine(minerFees) /// simplify
              println(s"BD NG fees: total = ${totalFees.balance}, miner = ${minerFees.balance}, total miner = ${totalMinerFees.balance}") ///
              (diff.copy(portfolios = diff.portfolios.combine(Map(blockGenerator -> totalMinerFees))), totalFees, constraint)
          }
    }

    txsDiffEi.map {
      case (d, fees, constraint) =>
        val diffWithCancelledLeases =
          if (currentBlockHeight == settings.resetEffectiveBalancesAtHeight)
            Monoid.combine(d, CancelAllLeases(composite(blockchain, d)))
          else d

        val diffWithLeasePatches =
          if (currentBlockHeight == settings.blockVersion3AfterHeight)
            Monoid.combine(diffWithCancelledLeases, CancelLeaseOverflow(composite(blockchain, diffWithCancelledLeases)))
          else diffWithCancelledLeases

        val diffWithCancelledLeaseIns =
          if (blockchain.featureActivationHeight(BlockchainFeatures.DataTransaction.id).contains(currentBlockHeight))
            Monoid.combine(diffWithLeasePatches, CancelInvalidLeaseIn(composite(blockchain, diffWithLeasePatches)))
          else diffWithLeasePatches

        (diffWithCancelledLeaseIns, fees, constraint)
    }
  }
}
