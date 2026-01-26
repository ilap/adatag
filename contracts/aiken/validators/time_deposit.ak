////    transaction_id: transaction.placeholder.id,

use aiken/collection/list
use aiken/interval
use cardano/script_context.{ScriptContext}
use cardano/transaction.{Transaction}
use ilap/adatag/tests/fixture.{collector_keyhash}
use ilap/adatag/types.{
  Collect, Credential, PosixTime, Redeem, TimeDepositDatum, TimeDepositParams,
  TimeDepositRedeemer,
}
use ilap/adatag/utils.{time_elapsed}

fn valid_claim(
  is_coll: Bool,
  datum: Data,
  cred: Credential,
  time: PosixTime,
  transaction: Transaction,
) -> Bool {
  let void_datum: Data = Void

  // Any void datum is handled as donation, meaning only the collector can claim it.
  // It's anti pattern but we allow any wrongly formed datums for collections
  let valid =
    if void_datum == datum {
      is_coll?
    } else {
      expect _d: TimeDepositDatum = datum
      time_elapsed(transaction.validity_range, time)?
    }

  let signed_by = list.has(transaction.extra_signatories, cred)?
  valid? && signed_by?
}

/// TimeDeposit validator is for redeeming or collect the locked deposit 
/// (a certain amount of ADA for some time e.g. 20 days, at adatag minting time)
/// to prevent for buying a lot of the rare usernames and sell them on the market for high price.
validator timedeposit(params: TimeDepositParams) {
  spend(
    datum: Option<Data>,
    rdmr: TimeDepositRedeemer,
    _own_ref: Void,
    transaction: Transaction,
  ) {
    expect Some(datum_data) = datum

    when rdmr is {
      // Collector can collect donations (txes with no TimeDepositDatum) anytime or
      // after the collection time with a TimeDepositDatum (unlcaimed time-lock deposits).
      Collect ->
        valid_claim(
          is_coll: True,
          datum: datum_data,
          cred: params.collector,
          time: params.collection_time,
          transaction: transaction,
        )
      // User can redeem only after the deadline has passed.
      // The false means it's not collection but redeem.
      Redeem -> {
        expect TimeDepositDatum { beneficiary, deadline }: TimeDepositDatum =
          datum_data

        valid_claim(
          is_coll: False,
          datum: datum_data,
          cred: beneficiary,
          time: deadline,
          transaction: transaction,
        )
      }
    }
  }

  else(_ctx: ScriptContext) {
    fail @"unsupported purpose"
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
  //let placeholder_utxo =
  //  OutputReference {
  //    output_index: 0,
  //  }
  let iv = interval.between(coll_time + 1000, coll_time + 2000)

  let tx =
    Transaction {
      ..transaction.placeholder,
      validity_range: iv,
      extra_signatories: [collector_keyhash],
    }

  //let context =
  //  ScriptContext { purpose: Spend(placeholder_utxo), transaction: tx }
  // FIXME: // cbor.diagnostic(Void) == @"a"
  //let o = option.choice([Some(datum)])
  //let o: Option<Data> = Some(datum)
  let o: Option<Data> = Some(as_data(datum))
  timedeposit.spend(params: params, datum: o, rdmr: rdmr, Void, transaction: tx)
  //True
}

test test_timelock_datums() {
  let cred = "00000000000000000"
  //let utxo =
  //  OutputReference {
  //   transaction_id: transaction.placeholder.id,
  //    output_index: 0,
  //  }
  let void_datum: Data = Void

  //let _valid_datum: TimeDepositDatum = TimeDepositDatum {
  //  beneficiary: cred,
  //  deadline: 0,
  //}
  let tx = Transaction { ..transaction.placeholder, extra_signatories: [cred] }

  //let ctx = ScriptContext { purpose: Spend(utxo), transaction: tx }
  valid_claim(True, void_datum, cred, 0, tx)?
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
