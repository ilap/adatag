use aiken/builtin
use aiken/crypto.{VerificationKeyHash}
use aiken/interval.{Interval}
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId, Value, add}
use cardano/transaction.{
  Datum, InlineDatum, Input, Output, OutputReference, Transaction,
}
use ilap/adatag/tests/fixture.{
  adahandle_policy, beneficiary_address, beneficiary_keyhash, collector_keyhash,
  minting_policy, state_holder_address, stateholder_hash, timedeposit_address,
  timedeposit_hash, token_policy,
}
use ilap/adatag/types.{
  AdatagAdded, AdatagRemoved, MintParams, State, TimeDepositDatum,
  TimeDepositParams,
}

/// ---------------------------------------------------------------------------
/// Global constants and helopers
/// ---------------------------------------------------------------------------
pub fn get_valid_deposit(base: Int, adatag: ByteArray) -> Int {
  let adatag_len = bytearray.length(adatag)
  when adatag_len is {
    1 -> base
    2 -> base / 2
    3 -> base / 4
    4 -> base / 8
    5 -> base / 16
    6 -> base / 32
    _ -> 15
  }
}

pub fn mk_valid_mint_value(adatag: ByteArray) -> Value {
  mk_asset(minting_policy, adatag, 1)
}

pub fn mk_valid_token_value(adatag: ByteArray) -> Value {
  mk_asset(token_policy, bytearray.take(adatag, 1), 1)
}

/// Minting policy
pub const mp = minting_policy

// Auth token policy hash
pub const tp = token_policy

pub const sv = stateholder_hash

pub const twenty_days = 20

pub const valid_timedeposit_value =
  assets.zero
    |> assets.add("", "", 218)

// pub const valid_timedeposit_datum =
//   TimeDepositDatum {
//     beneficiary: beneficiary_address,
//     deadline: twenty_days,
//   }

pub const valid_timedeposit_output =
  Output {
    address: timedeposit_address,
    value: valid_timedeposit_value,
    datum: InlineDatum(
      valid_timedeposit_datum(beneficiary_keyhash, twenty_days),
    ),
    reference_script: None,
  }

/// Auth token constants
/// ---------------------------------------------------------------------------
/// Timedeposit constants and helpers
/// ---------------------------------------------------------------------------
/// Only ADA is required for the timedeposit UTxO.
pub fn mk_timedeposit_tx(
  semi_open: Bool,
  from: Int,
  to: Int,
  sigs: List<VerificationKeyHash>,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    validity_range: if semi_open {
      interval.between(from, to)
    } else {
      interval.entirely_between(from, to)
    },
    extra_signatories: sigs,
  }
}

pub fn valid_timedeposit_datum(
  beneficiary: VerificationKeyHash,
  deadline: Int,
) -> Option<Data> {
  Some(as_data(TimeDepositDatum { beneficiary, deadline }))
}

pub fn invalid_datum() -> Option<Data> {
  Some(builtin.b_data(#"deadbeef"))
}

pub fn timedeposit_params(coll_time: Int) -> TimeDepositParams {
  TimeDepositParams { collector: collector_keyhash, collection_time: coll_time }
}

/// ---------------------------------------------------------------------------
/// Stateholder constants
/// ---------------------------------------------------------------------------
pub const adatag = "ilap"

pub const invalid_adatag = "ilap."

// Mint value
const mv = mk_asset(mp, adatag, 1)

// Mint negative value
pub const mnv = mk_asset(mp, adatag, -1)

// Auth token value
pub const av = mk_asset(tp, bytearray.take(adatag, 1), 1)

// Output value with auth token that contains minAda for the UTxO
pub const sov = av |> add("", "", 100)

// Tx constansts
pub const initial_state =
  State {
    operation_count: 0,
    operation: AdatagRemoved,
    size: "0",
    adatag: "",
    minting_policy: mp,
    root_hash: "",
  }

pub const second_state =
  State {
    ..initial_state,
    operation_count: initial_state.operation_count + 1,
    operation: AdatagAdded,
    size: "1",
    adatag: "ilap",
  }

const stateholder_utxo =
  Output {
    address: state_holder_address,
    value: av,
    datum: InlineDatum(initial_state),
    reference_script: None,
  }


pub const stateholder_intput_output =
  Output {
    address: state_holder_address,
    value: sov,
    datum: InlineDatum(initial_state),
    reference_script: None,
  }

pub const stateholder_output =
  Output {
    address: state_holder_address,
    value: sov,
    datum: InlineDatum(second_state),
    reference_script: None,
  }

pub const stateholder_ref =
  OutputReference {
    transaction_id: transaction.placeholder.id,
    output_index: 0,
  }

const own_input =
  Input { output_reference: stateholder_ref, output: stateholder_utxo }

pub const stateholder_tx =
  Transaction {
    ..transaction.placeholder,
    inputs: [own_input],
    outputs: [stateholder_output],
    mint: mv,
  }

/// ---------------------------------------------------------------------------
/// Adatag minting constants
/// ---------------------------------------------------------------------------
pub const deposit_base = 1750


pub const deactivation_time = 1000
pub const collection_time = 2000
pub const locking_days = 20


pub const time_before_collection = collection_time - 1
pub const time_after_collection = collection_time + 1
pub const time_before_deactivation = deactivation_time - 1
pub const time_after_deactivation = deactivation_time + 1

pub const before_deactivation_interval = interval.before(deactivation_time)
pub const after_deactivation_interval = interval.after(deactivation_time)
pub const before_collection_interval = interval.before(collection_time)
pub const after_collection_interval = interval.after(collection_time)



pub const mock_mint_params =
  MintParams {
    token_policy,
    stateholder_hash,
    timedeposit_hash,
    deposit_base,
    deactivation_time,
    locking_days,
    adahandle: adahandle_policy,
  }

/// Helper functions for tests
pub fn mk_outref(index: Int) -> OutputReference {
  OutputReference {
    transaction_id: transaction.placeholder.id,
    output_index: index,
  }
}

pub fn mk_input(outref_index: Int, output: Output) -> Input {
  Input { output_reference: mk_outref(index: outref_index), output }
}

pub fn mk_output(address: Address, value: Value, datum: Datum) -> Output {
  Output { address, value, datum, reference_script: None }
}

pub fn mk_asset(pid: PolicyId, asset_name: AssetName, amount: Int) -> Value {
  assets.zero |> add(pid, asset_name, amount)
}
