use aiken/interval
use aiken/list
use aiken/time.{PosixTime}
use aiken/transaction.{
  OutputReference, ScriptContext, Spend, Transaction, TransactionId,
}
use ilap/adatag/tests/fixture.{collector_keyhash}
use ilap/adatag/types.{
  Collect, Credential, Redeem, TimeDepositDatum, TimeDepositParams,
  TimeDepositRedeemer,
}
use ilap/adatag/utils.{time_elapsed}

fn valid_claim(
  is_coll: Bool,
  datum: Data,
  cred: Credential,
  time: PosixTime,
  context: ScriptContext,
) -> Bool {
  let void_datum: Data = Void

  // Any void datum is handled as donation, meaning only the collector can claim it.
  // It's anti pattern but we allow any wrongly formed datums for collections
  let valid =
    if void_datum == datum {
      is_coll?
    } else {
      expect _d: TimeDepositDatum = datum
      time_elapsed(context.transaction.validity_range, time)?
    }

  let signed_by = list.has(context.transaction.extra_signatories, cred)?
  valid? && signed_by?
}

/// TimeDeposit validator is for redeeming or collect the locked deposit 
/// (a certain amount of ADA for some time e.g. 20 days, at adatag minting time)
/// to prevent for buying a lot of the rare usernames and sell them on the market for high price.
validator(params: TimeDepositParams) {
  fn timedeposit(
    datum: Data,
    rdmr: TimeDepositRedeemer,
    ctx: ScriptContext,
  ) -> Bool {
    when rdmr is {
      // Collector can collect donations (txes with no TimeDepositDatum) anytime or
      // after the collection time with a TimeDepositDatum (unlcaimed time-lock deposits).
      Collect ->
        valid_claim(
          is_coll: True,
          datum: datum,
          cred: params.collector,
          time: params.collection_time,
          context: ctx,
        )
      // User can redeem only after the deadline has passed.
      // The false means it's not collection but redeem.
      Redeem -> {
        expect TimeDepositDatum { beneficiary, deadline }: TimeDepositDatum =
          datum

        valid_claim(
          is_coll: False,
          datum: datum,
          cred: beneficiary,
          time: deadline,
          context: ctx,
        )
      }
    }
  }
}

/// aiken check -e -m "test_timedeposit_validator"
test test_timedeposit_validator() {
  let coll_time = 10000
  let params =
    TimeDepositParams {
      collector: collector_keyhash,
      collection_time: coll_time,
    }

  //let datum = debug(Void) //VoidDatum 
  let datum =
    TimeDepositDatum { beneficiary: collector_keyhash, deadline: coll_time }
  let rdmr = Redeem

  // Collect
  let placeholder_utxo =
    OutputReference { transaction_id: TransactionId(""), output_index: 0 }

  let iv = interval.between(coll_time + 1000, coll_time + 2000)

  let tx =
    Transaction {
      ..transaction.placeholder(),
      validity_range: iv,
      extra_signatories: [collector_keyhash],
    }
  let context =
    ScriptContext { purpose: Spend(placeholder_utxo), transaction: tx }

  // FIXME: // cbor.diagnostic(Void) == @"a"
  timedeposit(params: params, datum: datum, rdmr: rdmr, ctx: context)
  //True
}

test test_timelock_datums() {
  let cred = "00000000000000000"
  let utxo =
    OutputReference { transaction_id: TransactionId(""), output_index: 0 }
  let void_datum: Data = Void

  //let _valid_datum: TimeDepositDatum = TimeDepositDatum {
  //  beneficiary: cred,
  //  deadline: 0,
  //}
  let tx =
    Transaction { ..transaction.placeholder(), extra_signatories: [cred] }
  let ctx = ScriptContext { purpose: Spend(utxo), transaction: tx }

  valid_claim(True, void_datum, cred, 0, ctx)?
  //&& valid_claim(True, valid_datum, cred, 0, ctx)? 
}
// FIXME: test the followings:
// 1. Collect & Void Datum: PASS Anytime
// 2. Collect & Valid Datum
//  - Before coll time: Fail
//  - At coll time: Pass
//  - After coll time: Pass

//
// 3. Redeem & Void Datum: FAIL Anytime

//    
// 4. Redeem & Valid Datum
//  - Before deadline: Fail
//  - At deadline: Pass
//  - After deadline: Pass
//  - Beneficiary is not the Signatory: Fail
