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

  private def clearNgAndSponsorship(blockchain: Blockchain,
                                    portfolio: Portfolio,
                                    height: Int,
                                    fs: FunctionalitySettings): (Portfolio, Option[Portfolio]) = {
    val ngHeight          = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)
    val sponsorshipHeight = Sponsorship.sponsoredFeesSwitchHeight(blockchain, fs)
    val pf =
      if (height >= sponsorshipHeight)
        Portfolio.empty.copy(
          balance = portfolio.balance +
            portfolio.assets.map {
              case (assetId, assetFee) =>
                val baseFee = blockchain.assetDescription(assetId).get.sponsorship
                Sponsorship.toWaves(assetFee, baseFee)
            }.sum)
      else portfolio

    if (height >= ngHeight) {
      val curPf  = pf.multiply(Block.CurrentBlockFeePart)
      val nextPf = pf.minus(curPf)
      (curPf, Some(nextPf))
    } else (pf, None)
  }

  def fromBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                blockchain: Blockchain,
                                                maybePrevBlock: Option[Block],
                                                block: Block,
                                                constraint: Constraint): Either[ValidationError, (Diff, Option[Portfolio], Constraint)] = {
    val stateHeight = blockchain.height

    // height switch is next after activation
    val ngHeight          = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)
    val sponsorshipHeight = Sponsorship.sponsoredFeesSwitchHeight(blockchain, settings)

    lazy val prevBlockFeeDistr: Option[Portfolio] =
      if (stateHeight >= sponsorshipHeight)
        blockchain.carryFee
      else if (stateHeight > ngHeight)
        maybePrevBlock.map(_.prevBlockFeePart())
      else None

    lazy val currentBlockFeeDistr: Option[Portfolio] =
      if (blockchain.height < ngHeight)
        Some(block.feesPortfolio())
      else
        None

    for {
      _ <- block.signaturesValid()
      r <- apply(
        settings,
        blockchain,
        constraint,
        maybePrevBlock.map(_.timestamp),
        block.signerData.generator,
        prevBlockFeeDistr,
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
                                                     constraint: Constraint): Either[ValidationError, (Diff, Option[Portfolio], Constraint)] = {
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
                                                    heightDiff: Int): Either[ValidationError, (Diff, Option[Portfolio], Constraint)] = {
    def updateConstraint(constraint: Constraint, blockchain: Blockchain, tx: Transaction): Constraint =
      constraint.put(blockchain, tx).asInstanceOf[Constraint]

    val currentBlockHeight                          = blockchain.height + heightDiff
    val txDiffer                                    = TransactionDiffer(settings, prevBlockTimestamp, timestamp, currentBlockHeight) _
    val initDiff                                    = Diff.empty.copy(portfolios = Map(blockGenerator -> currentBlockFeeDistr.orElse(prevBlockFeeDistr).orEmpty))
    val init: (Diff, Option[Portfolio], Constraint) = (initDiff, None, initConstraint)
    val hasNG                                       = currentBlockFeeDistr.isEmpty

    txs
      .foldLeft(init.asRight[ValidationError]) {
        case (r @ Left(_), _) => r
        case (Right((currDiff, carryFee, currConstraint)), tx) =>
          val updatedBlockchain = composite(blockchain, currDiff)
          val updatedConstraint = updateConstraint(currConstraint, updatedBlockchain, tx)
          if (updatedConstraint.isOverfilled)
            Left(ValidationError.GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint"))
          else
            txDiffer(updatedBlockchain, tx).map { newDiff =>
              val updatedDiff = currDiff.combine(newDiff)
              if (hasNG) {
                val (curPf, nextPf) = clearNgAndSponsorship(updatedBlockchain, tx.feeDiff(), currentBlockHeight, settings)
                val diff            = updatedDiff.combine(Diff.empty.copy(portfolios = Map(blockGenerator -> curPf)))
                val carry           = carryFee.flatMap(pf => nextPf.map(_.combine(pf))).orElse(nextPf)
                (diff, carry, updatedConstraint)
              } else (updatedDiff, None, updatedConstraint)
            }
      }
      .map {
        case (d, carryFee, constraint) =>
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

          (diffWithCancelledLeaseIns, carryFee, constraint)
      }
  }
}
