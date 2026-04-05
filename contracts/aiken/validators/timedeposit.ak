use aiken/collection/list
use aiken/option.{is_none}
use cardano/transaction.{Transaction}
use ilap/adatag/types.{
  Collect, Redeem, TimeDepositDatum, TimeDepositParams, TimeDepositRedeemer,
}
use ilap/adatag/utils.{decode_timedeposit_datum, time_elapsed}

/// Timedeposit Validator
///
/// This validator enforces time- and signer-based conditions on a time deposit UTxO.
///
/// ## Collect
/// Allows a user to collect the deposit under the following conditions:
/// 1. **Invalid or missing datum**: the collector can collect at any time.
/// 2. **Valid datum**: the collector can collect only if the `collection_time` has passed.
///
/// **Note:** In both cases, the collector must be a witness/signatory of the transaction.
///
/// ## Redeem
/// Allows the deposit to be redeemed by the beneficiary under the following conditions:
/// 1. **Valid datum**: the redeemer can redeem only if the `deadline` has passed.
/// 2. The beneficiary must be a witness/signatory of the transaction.
///
/// ## Assumptions
/// - All time checks are performed according to Plutus V3 semi-open interval semantics
///   `[lower_bound, upper_bound)`; upper bound is exclusive.
/// - Ledger guarantees: `lower_bound ≤ upper_bound`.
/// - The `time_elapsed` predicate determines whether the current transaction slot
///   is strictly after the relevant time (`collection_time` or `deadline`).
/// - Invalid datum does not prevent collection; valid datum is enforced for Redeem.
validator timedeposit(params: TimeDepositParams) {
  spend(
    datum: Option<Data>,
    rdmr: TimeDepositRedeemer,
    _,
    transaction: Transaction,
  ) {
    let td: Option<TimeDepositDatum> = decode_timedeposit_datum(datum)

    when rdmr is {
      Collect -> and {
          or {
            td |> is_none(),
            time_elapsed(transaction.validity_range, params.collection_time)?,
          },
          list.has(transaction.extra_signatories, params.collector)?,
        }

      Redeem -> {
        expect Some(TimeDepositDatum { beneficiary, deadline }) = td
        and {
          time_elapsed(transaction.validity_range, deadline)?,
          list.has(transaction.extra_signatories, beneficiary)?,
        }
      }
    }
  }

  else(_) {
    fail @"unsupported purpose"
  }
}
