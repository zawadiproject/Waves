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

  private def clearNgAndSponsorship(blockchain: Blockchain, portfolio: Portfolio, height: Int, fs: FunctionalitySettings): (Portfolio, Portfolio) = {
    if (height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain, fs)) {
      val minerFee = portfolio.assets.map {
        case (assetId, assetFee) =>
          val baseFee = blockchain.assetDescription(assetId).get.sponsorship ///get
          Sponsorship.toWaves(assetFee, baseFee)
      }.sum
      val totalPf = Portfolio.empty.copy(balance = portfolio.balance + minerFee)
      val curPf   = totalPf.multiply(Block.CurrentBlockFeePart)
      val nextPf  = totalPf.minus(curPf)
//      println(s"BD clear ${curPf.balance} : ${nextPf.balance}")///
      (curPf, nextPf)
    } else (portfolio.multiply(Block.CurrentBlockFeePart), Portfolio.empty) ///, None
  }

  def fromBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                blockchain: Blockchain,
                                                maybePrevBlock: Option[Block],
                                                block: Block,
                                                constraint: Constraint): Either[ValidationError, (Diff, Portfolio, Constraint)] = {
    val stateHeight = blockchain.height

    // height switch is next after activation
    val ngHeight          = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)
    val sponsorshipHeight = Sponsorship.sponsoredFeesSwitchHeight(blockchain, settings)

    lazy val prevBlockFeeDistr: Option[Portfolio] =
      if (stateHeight >= sponsorshipHeight) {
        if (stateHeight > ngHeight) {
          val z = blockchain.carryFee
          println(s"> SP < $z") ///
          z
        } else {
          blockchain.carryFee
        }
      } else if (stateHeight > ngHeight) {
        println(s"> NG < ${maybePrevBlock.map(_.prevBlockFeePart())}") ///
        maybePrevBlock.map(_.prevBlockFeePart())
      } else {
//        println(s"> -- < None") ///
        None
      }

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
    } yield {
//      println(s"BD h=$stateHeight next block fees = ${r._2.balance}") ///
      r
    }
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
        val prevBlockFees = prevBlockFeeDistr.orEmpty ///?
        println(s"BD NG    h=${blockchain.height}, prev block fee = ${prevBlockFees.balance} WAVES") ///
        val initDiff = Diff.empty.copy(portfolios = Map(blockGenerator -> prevBlockFees))
        txs.foldLeft((initDiff, Portfolio.empty, initConstraint).asRight[ValidationError]) {
          case (r @ Left(_), _) => r
          case (Right((currDiff, nextBlockFees, currConstraint)), tx) =>
            val updatedBlockchain = composite(blockchain, currDiff)
            val updatedConstraint = updateConstraint(currConstraint, updatedBlockchain, tx)
            if (updatedConstraint.isOverfilled)
              Left(ValidationError.GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint"))
            else
              txDiffer(updatedBlockchain, tx).map { newDiff =>
                val (curPf, nextPf) = clearNgAndSponsorship(updatedBlockchain, tx.feeDiff(), currentBlockHeight, settings)
                println(s"BD NG    tx fee = ${curPf.balance}, next fee = ${nextPf.balance}") ///
                (currDiff.combine(newDiff.copy(portfolios = newDiff.portfolios.combine(Map(blockGenerator -> curPf)))),
                 nextBlockFees.combine(nextPf),
                 updatedConstraint)
              }
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
