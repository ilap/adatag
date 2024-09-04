use aiken/collection/list
use aiken/interval
use cardano/address.{Credential, VerificationKey}
use cardano/transaction.{OutputReference, Transaction}
use ilap/adatag/tests/fixture.{collector_keyhash}
use ilap/adatag/types.{
  Collect, Redeem, TimeDepositDatum, TimeDepositParams, TimeDepositRedeemer,
}
use ilap/adatag/utils.{time_elapsed}

/// Handles claims based on the validity of the provided datum. 
///
/// If the datum is not of type `TimeDepositDatum` (or is absent), it is
/// treated as a donation, meaning only the collector can claim it. This
/// approach is non-ideal, but it allows collection of incorrectly
/// formatted datums to avoid losing locked deposits.
///
/// The `Redeem` claim is valid only under the following conditions:
/// - The deadline has passed and the beneficiary is the one signing the
///   transaction.
///
/// The `Collect` claim is valid in either of the following cases:
/// - The datum is invalid or
/// - The datum is valid and the collection time has passed.
/// - The collector is the one signing the transaction.
fn valid_claim(
  is_coll: Bool,
  datum: Option<TimeDepositDatum>,
  cred: Credential,
  deadline: Int,
  transaction: Transaction,
) -> Bool {
  let valid =
    when datum is {
      None -> is_coll?
      Some(_) -> time_elapsed(transaction.validity_range, deadline)?
    }

  let signed_by =
    when cred is {
      VerificationKey(cred) -> list.has(transaction.extra_signatories, cred)?
      // FIXME(ilap): Handle the case when the credential is a script
      _ -> fail @"We only support VerificationKey for now"
    }
  valid? && signed_by?
}

/// TimeDeposit validator is for redeeming or collect the locked deposit 
/// (a certain amount of ADA for some time e.g. 20 days, at adatag minting time)
/// to prevent for buying a lot of the rare usernames and sell them on the market for high price.
validator timedeposit(params: TimeDepositParams) {
  spend(
    datum: Option<TimeDepositDatum>,
    rdmr: TimeDepositRedeemer,
    _input: OutputReference,
    transaction: Transaction,
  ) {
    when rdmr is {
      // Collector can collect donations (txes with no TimeDepositDatum) anytime or
      // after the collection time with a TimeDepositDatum (unlcaimed time-lock deposits).
      Collect ->
        valid_claim(
          is_coll: True,
          datum: datum,
          cred: params.collector,
          deadline: params.collection_time,
          transaction: transaction,
        )
      // User can redeem only after the deadline has passed.
      // The false means it's not collection but redeem.
      Redeem -> {
        // For redeem the user must provide a valid TimeDepositDatum
        expect Some(data) = datum
        let TimeDepositDatum { beneficiary, deadline }: TimeDepositDatum = data

        valid_claim(
          is_coll: False,
          datum: Some(data),
          cred: beneficiary,
          deadline: deadline,
          transaction: transaction,
        )
      }
    }
  }

  else(_) {
    fail
  }
}

/// aiken check -e -m "test_timedeposit_validator"
test test_timedeposit_validator() {
  let coll_time = 10000
  let params =
    TimeDepositParams {
      collector: VerificationKey(collector_keyhash),
      collection_time: coll_time,
    }

  //let datum = debug(Void) //VoidDatum 
  let datum =
    TimeDepositDatum {
      beneficiary: VerificationKey(collector_keyhash),
      deadline: coll_time,
    }
  let rdmr = Redeem

  // Collect
  let placeholder_utxo =
    OutputReference { transaction_id: #"", output_index: 0 }

  let iv = interval.between(coll_time + 1000, coll_time + 2000)

  let tx =
    Transaction {
      ..transaction.placeholder,
      validity_range: iv,
      extra_signatories: [collector_keyhash],
    }

  // let context = ScriptContext { purpose: Spend(placeholder_utxo), transaction: tx }
  // FIXME: // cbor.diagnostic(Void) == @"a"
  timedeposit.spend(params, Some(datum), rdmr, placeholder_utxo, tx)
  //True
}

test test_timelock_datums() {
  let cred = #"000000000000000000"

  let valid_datum: TimeDepositDatum =
    TimeDepositDatum { beneficiary: VerificationKey(cred), deadline: 10 }
  let tx =
    Transaction {
      ..transaction.placeholder,
      validity_range: interval.between(0, 11),
      extra_signatories: [cred],
    }

  let redeem = False

  //let collect = !redeem
  valid_claim(redeem, Some(valid_datum), VerificationKey(cred), 0, tx)?
  // && // Reedeem before deadline
  //valid_claim(redeem, Some(valid_datum), cred, 10, tx)? && // Reedeem on deadline
  //!valid_claim(redeem, None, cred, 0, tx)? && // Collect
  //valid_claim(collect, None, cred, 0, tx)?
  // FIXME: && valid_claim(collect, Some(valid_datum), cred, 11, tx)?
}
/// Redeem
/// Time has passes
// FIXME: test the followings:
// 1. Collect & Void Datum: PASS Anytime
// 2. Collect & Valid Datum
//  - Before coll deadline: Fail
//  - At coll deadline: Pass
//  - After coll deadline: Pass

//
// 3. Redeem & Void Datum: FAIL Anytime

//    
// 4. Redeem & Valid Datum
//  - Before deadline: Fail
//  - At deadline: Pass
//  - After deadline: Pass
//  - Beneficiary is not the Signatory: Fail
