package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.state.{EitherExt2, LeaseBalance, Portfolio, Sponsorship}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.transfer._

class TransferTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val preconditionsAndTransfer: Gen[(GenesisTransaction, IssueTransaction, IssueTransaction, TransferTransaction)] = for {
    master    <- accountGen
    recepient <- otherAccountGen(candidate = master)
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
    issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
    maybeAsset               <- Gen.option(issue1)
    maybeAsset2              <- Gen.option(issue2)
    maybeFeeAsset            <- Gen.oneOf(maybeAsset, maybeAsset2)
    transferV1               <- transferGeneratorP(master, recepient, maybeAsset.map(_.id()), maybeFeeAsset.map(_.id()))
    transferV2               <- versionedTransferGeneratorP(master, recepient, maybeAsset.map(_.id()), maybeFeeAsset.map(_.id()))
    transfer                 <- Gen.oneOf(transferV1, transferV2)
  } yield (genesis, issue1, issue2, transfer)

  property("transfers assets to recipient preserving waves invariant") {
    forAll(preconditionsAndTransfer) {
      case ((genesis, issue1, issue2, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, issue1, issue2))), TestBlock.create(Seq(transfer))) {
          case (totalDiff, newState) =>
            assertBalanceInvariant(totalDiff)

            val recipient: Address = transfer.recipient.asInstanceOf[Address]
            val recipientPortfolio = newState.portfolio(recipient)
            if (transfer.sender.toAddress != recipient) {
              transfer.assetId match {
                case Some(aid) => recipientPortfolio shouldBe Portfolio(0, LeaseBalance.empty, Map(aid -> transfer.amount))
                case None      => recipientPortfolio shouldBe Portfolio(transfer.amount, LeaseBalance.empty, Map.empty)
              }
            }
        }
    }
  }

  val transferWithSmartAssetFee: Gen[(GenesisTransaction, IssueTransaction, IssueTransactionV2, TransferTransaction)] = {
    for {
      master    <- accountGen
      recepient <- otherAccountGen(master)
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      issue: IssueTransaction      <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
      feeIssue: IssueTransactionV2 <- smartIssueTransactionGen(master, scriptGen.map(_.some))
      transferV1                   <- transferGeneratorP(master, recepient, issue.id().some, feeIssue.id().some)
      transferV2                   <- transferGeneratorP(master, recepient, issue.id().some, feeIssue.id().some)
      transfer                     <- Gen.oneOf(transferV1, transferV2)
    } yield (genesis, issue, feeIssue, transfer)
  }

  property("fails, if smart asset used as a fee") {
    import smart._

    forAll(transferWithSmartAssetFee) {
      case (genesis, issue, fee, transfer) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(issue, fee)), smartEnabledFS) {
          case (_, state) => {
            val diffOrError = TransferTransactionDiff(state, smartEnabledFS, System.currentTimeMillis(), state.height)(transfer)
            diffOrError shouldBe Left(GenericError("Smart assets can't participate in TransferTransactions as a fee"))
          }
        }
    }
  }

  property("///") {
    val setup = for {
      acc <- accountGen
      ts  <- positiveIntGen
      genesis  = GenesisTransaction.create(acc, ENOUGH_AMT, ts).explicitGet()
      transfer = TransferTransactionV1.selfSigned(None, acc, acc, 1, ts + 10, None, 100000, Array()).explicitGet()
    } yield (acc, genesis, transfer)

    val fs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(NG.id -> 0))
    setup.sample match {
      case Some((acc, genesis, xfer)) =>
        val b1 = TestBlock.create(acc, Seq(genesis))
        val b2 = TestBlock.create(acc, Seq(xfer))
        val b3 = TestBlock.create(acc, Seq.empty)

        println(s"b1 $ENOUGH_AMT")
        assertDiffAndState(Seq(b1), b2, fs) {
          case (diff, state) =>
            println(s"b2 ${state.balance(acc, None) - ENOUGH_AMT}")
        }
        assertDiffAndState(Seq(b1, b2), b3, fs) {
          case (diff, state) =>
            println(s"b3 ${state.balance(acc, None) - ENOUGH_AMT}")
        }
    }
  }
//  property("sponsorship fees are calculated correctly") {
//    val setup: Gen[(PrivateKeyAccount, GenesisTransaction, IssueTransaction, SponsorFeeTransaction, TransferTransaction, TransferTransaction)] = {
//      val Waves = 100000000
//      val Sp    = Sponsorship.FeeUnit * 10
//      for {
//        master <- accountGen
//        ts     <- positiveIntGen
//        genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
//        issue   = IssueTransactionV1.selfSigned(master, "homa".getBytes, Array.emptyByteArray, ENOUGH_AMT, 2, false, Waves, ts + 100).explicitGet()
////        issue <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
//        sponsorship = SponsorFeeTransaction.selfSigned(1, master, issue.id(), Some(Sp), 100000000, ts + 200).explicitGet()
//        transfer1   = TransferTransactionV1.selfSigned(None, master, master, 1, ts + 300, Some(issue.id()), Sp + 7, Array.emptyByteArray).explicitGet()
//        transfer2   = TransferTransactionV1.selfSigned(None, master, master, 1, ts + 400, Some(issue.id()), Sp + 7, Array.emptyByteArray).explicitGet()
////        transfer1                   <- transferGeneratorP(master, master, None, issue.id().some)
////        transfer2                   <- transferGeneratorP(master, master, None, issue.id().some)
//      } yield (master, genesis, issue, sponsorship, transfer1, transfer2)
//    }
//    val fs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(NG.id -> 0, FeeSponsorship.id -> 0),
//                                                    blocksForFeatureActivation = 1,
//                                                    featureCheckBlocksPeriod = 1)
//
////    forAll(setup) {
//    setup.sample match {
//      case Some((master, genesis, issue, sponsorship, transfer1, transfer2)) =>
//        val block0  = TestBlock.create(master, Seq(genesis, issue, sponsorship))
//        val block1  = TestBlock.create(master, Seq(transfer1))
//        val block2  = TestBlock.create(master, Seq(transfer2))
//        val block12 = TestBlock.create(master, Seq(transfer1, transfer2))
//        val blocke  = TestBlock.create(master, Seq.empty)
//        val blockee = TestBlock.create(master, Seq.empty)
//        assertDiffAndState(Seq(block0, block12), blocke, fs) {
//          case (diff, state) =>
//            println(s"master ${master.address} had $ENOUGH_AMT")
//            println(s"diff ${diff.portfolios}")
//            println(s"state w = ${state.balance(master, None) - ENOUGH_AMT}")
//            println(s"state a = ${state.balance(master, Some(issue.id())) - ENOUGH_AMT}")
//        }
//    }
//  }
}
