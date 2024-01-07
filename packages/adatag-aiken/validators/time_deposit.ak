use aiken/cbor
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
use ilap/adatag/utils.{debug, time_elapsed}

//use aiken/cbor

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
  trace @"Alma"
  {
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
          debug(datum)

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

test test_timelock_datum() {
  let utxo =
    OutputReference { transaction_id: TransactionId(""), output_index: 0 }

  //let input = Input { output_reference: utxo, output }
  let tx = Transaction { ..transaction.placeholder() }
  let ctx = ScriptContext { purpose: Spend(utxo), transaction: tx }

  let void_datum: Data = Void
  let a = Void
  valid_claim(True, Void, "00", 0, ctx)
  //cbor.diagnostic(a) == @"a"
}
