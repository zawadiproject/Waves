package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.settings.{Constants, TestFunctionalitySettings}
import com.wavesplatform.state.diffs._
import com.wavesplatform.state.{ByteStr, EitherExt2}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.transaction.assets.{IssueTransactionV1, SponsorFeeTransaction}
import com.wavesplatform.transaction.transfer.TransferTransactionV1

class SponsorFeeTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {
  val One = 100000000L
  val NgAndSponsorshipSettings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(NG.id -> 0, FeeSponsorship.id -> 0),
                                                                        blocksForFeatureActivation = 1,
                                                                        featureCheckBlocksPeriod = 1)

  property("SponsorFee serialization roundtrip") {
    forAll(sponsorFeeGen) { transaction: SponsorFeeTransaction =>
      val recovered = SponsorFeeTransaction.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

  property("SponsorFee serialization from TypedTransaction") {
    forAll(sponsorFeeGen) { transaction: SponsorFeeTransaction =>
      val recovered = TransactionParsers.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

  property("JSON format validation") {
    val js = Json.parse(s"""{
 "type": 14,
 "id": "Gobt7AiyQAfduRkW8Mk3naWbzH67Zsv9rdmgRNmon1Mb",
 "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
 "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
 "fee": $One,
 "timestamp": 1520945679531,
 "proofs": [
 "3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"
 ],
 "version": 1,
 "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
 "minSponsoredAssetFee": 100000
                       }
    """)

    val tx = SponsorFeeTransaction
      .create(
        1,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get,
        Some(100000),
        One,
        1520945679531L,
        Proofs(Seq(ByteStr.decodeBase58("3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7").get))
      )
      .right
      .get
    js shouldEqual tx.json()
  }

  val invalidFee =
    Table(
      "fee",
      -1 * Constants.UnitsInWave,
      0
    )

  property("sponsorship negative fee") {
    forAll(invalidFee) { fee: Long =>
      for {
        sender                                                                       <- accountGen
        (_, assetName, description, quantity, decimals, reissuable, iFee, timestamp) <- issueParamGen
        issue = IssueTransactionV1
          .selfSigned(sender, assetName, description, quantity, decimals, reissuable = reissuable, iFee, timestamp)
          .right
          .get
        minFee <- smallFeeGen
        assetId = issue.assetId()
      } yield SponsorFeeTransaction.selfSigned(1, sender, assetId, Some(minFee), fee, timestamp) should produce("insufficient fee")
    }
  }

  property("cancel sponsorship negative fee") {
    forAll(invalidFee) { fee: Long =>
      for {
        sender                                                                       <- accountGen
        (_, assetName, description, quantity, decimals, reissuable, iFee, timestamp) <- issueParamGen
        issue = IssueTransactionV1
          .selfSigned(sender, assetName, description, quantity, decimals, reissuable = reissuable, iFee, timestamp)
          .right
          .get
        minFee  = None
        assetId = issue.assetId()
      } yield SponsorFeeTransaction.selfSigned(1, sender, assetId, minFee, fee, timestamp) should produce("insufficient fee")
    }
  }

  property("miner receives one satoshi less than sponsor pays") {
    val Sp = 6
    val setup = for {
      acc <- accountGen
      ts  <- positiveIntGen
      genesis  = GenesisTransaction.create(acc, ENOUGH_AMT, ts).explicitGet()
      issue    = IssueTransactionV1.selfSigned(acc, "asset".getBytes("UTF-8"), Array(), ENOUGH_AMT, 0, false, One, ts + 10).explicitGet()
      sponsor  = SponsorFeeTransaction.selfSigned(1, acc, issue.id(), Some(Sp), One, ts + 20).explicitGet()
      transfer = TransferTransactionV1.selfSigned(None, acc, acc, 1, ts + 30, Some(issue.id()), Sp, Array()).explicitGet()
    } yield (acc, genesis, issue, sponsor, transfer)

    setup.sample match { ///forAll
      case Some((acc, genesis, issue, sponsor, transfer)) =>
        val b0 = TestBlock.create(acc, Seq(genesis, issue, sponsor))
        val b1 = TestBlock.create(acc, Seq(transfer))
        val b2 = TestBlock.create(acc, Seq.empty)

        assertDiffAndState(Seq(b0, b1), b2, NgAndSponsorshipSettings) {
          case (diff, state) =>
            println(s"b2 ${state.balance(acc, None) - ENOUGH_AMT}")
        }
    }
  }

  property("miner receives one satoshi more than sponsor pays") {
    val Sp = 1000000
    val setup = for {
      acc <- accountGen
      ts  <- positiveIntGen
      genesis   = GenesisTransaction.create(acc, ENOUGH_AMT, ts).explicitGet()
      issue     = IssueTransactionV1.selfSigned(acc, "asset".getBytes("UTF-8"), Array(), ENOUGH_AMT, 0, false, One, ts + 10).explicitGet()
      sponsor   = SponsorFeeTransaction.selfSigned(1, acc, issue.id(), Some(Sp), One, ts + 20).explicitGet()
      transfer1 = TransferTransactionV1.selfSigned(None, acc, acc, 1, ts + 100, Some(issue.id()), Sp + 7, Array()).explicitGet()
      transfer2 = TransferTransactionV1.selfSigned(None, acc, acc, 1, ts + 110, Some(issue.id()), Sp + 7, Array()).explicitGet()
    } yield (acc, genesis, issue, sponsor, transfer1, transfer2)

    setup.sample match { ///forAll
      case Some((acc, genesis, issue, sponsor, transfer1, transfer2)) =>
        val b0 = TestBlock.create(acc, Seq(genesis, issue, sponsor))
        val b1 = TestBlock.create(acc, Seq(transfer1, transfer2))
        val b2 = TestBlock.create(acc, Seq.empty)

        assertDiffAndState(Seq(b0, b1), b2, NgAndSponsorshipSettings) {
          case (diff, state) =>
            println(s"b2 ${state.balance(acc, None) - ENOUGH_AMT}")
        }
    }
  }

  property("sponsorship changes in the middle of a block") {
    val setup = for {
      acc <- accountGen
      ts  <- positiveIntGen
      genesis   = GenesisTransaction.create(acc, ENOUGH_AMT, ts).explicitGet()
      issue     = IssueTransactionV1.selfSigned(acc, "asset".getBytes("UTF-8"), Array(), ENOUGH_AMT, 0, false, One, ts + 10).explicitGet()
      sponsor1  = SponsorFeeTransaction.selfSigned(1, acc, issue.id(), Some(10), One, ts + 100).explicitGet()
      transfer1 = TransferTransactionV1.selfSigned(None, acc, acc, 1, ts + 110, Some(issue.id()), 10, Array()).explicitGet()
      sponsor2  = SponsorFeeTransaction.selfSigned(1, acc, issue.id(), Some(100), One, ts + 200).explicitGet()
      transfer2 = TransferTransactionV1.selfSigned(None, acc, acc, 1, ts + 210, Some(issue.id()), 100, Array()).explicitGet()
    } yield (acc, genesis, issue, sponsor1, transfer1, sponsor2, transfer2)

    setup.sample match { ///forAll
      case Some((acc, genesis, issue, sponsor1, transfer1, sponsor2, transfer2)) =>
        val b0 = TestBlock.create(acc, Seq(genesis, issue, sponsor1))
        val b1 = TestBlock.create(acc, Seq(transfer1, sponsor2, transfer2))
        val b2 = TestBlock.create(acc, Seq.empty)

        assertDiffAndState(Seq(), b0, NgAndSponsorshipSettings) {
          case (diff, state) =>
            println(s"b0 ${state.balance(acc, None) - ENOUGH_AMT}")
        }
        assertDiffAndState(Seq(b0), b1, NgAndSponsorshipSettings) {
          case (diff, state) =>
            println(s"b1 ${state.balance(acc, None) - ENOUGH_AMT}")
        }
        assertDiffAndState(Seq(b0, b1), b2, NgAndSponsorshipSettings) {
          case (diff, state) =>
            println(s"b2 ${state.balance(acc, None) - ENOUGH_AMT}")
        }
    }
  }
}
