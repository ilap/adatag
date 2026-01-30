use aiken/primitive/bytearray
use cardano/assets.{add}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction,
}
use ilap/adatag/tests/fixture.{
  another_hash, other_hash, own_hash, own_scriptaddress,
}
use ilap/adatag/types.{AdatagAdded, AdatagRemoved, State}
use state_holder
use tests/test_utils.{mk_asset}

// StateHolder validator hash
// TODO: const sp = own_hash

// Minting policy hash
/// The initial state must be correct at bootstrap and the valid state 
/// transitions are enforced by the validator scripts. The validator
/// does not check the state proof's correctness only that the state transition
/// is valid according to the rules defined.
///
/// Assumptions for a proper transaction:
/// - spend exactly one input from this script containing the auth token and a valid inline state datum
/// - produce exactly one output to this script containing the auth token and an updated inline state datum
/// - assuming that the validator out_ref parameter points to the input being spent forced by the ledger rules.
const mp = other_hash

// Auth token policy hash
const ap = another_hash

const adatag = "ilap"

// TODO: const invalid_adatag = "_ilap"

// Mint value
const mv = mk_asset(mp, adatag, 1)

// Mint negative value
// TODO: const mnv = mk_asset(mp, adatag, -1)

// Auth token value
const av = mk_asset(ap, bytearray.take(adatag, 1), 1)

// Output value with auth token that contains minAda for the UTxO
const ov = av |> add("", "", 100)

// Tx constansts
const initial_state =
  State {
    operation_count: 0,
    operation: AdatagRemoved,
    size: "0",
    adatag: "",
    minting_policy: mp,
    root_hash: "",
  }

const second_state =
  State {
    ..initial_state,
    operation_count: initial_state.operation_count + 1,
    operation: AdatagAdded,
    size: "1",
    adatag: "ilap",
  }

const own_utxo =
  Output {
    address: own_scriptaddress,
    value: av,
    datum: InlineDatum(initial_state),
    reference_script: None,
  }

const own_output =
  Output {
    address: own_scriptaddress,
    value: ov,
    datum: InlineDatum(second_state),
    reference_script: None,
  }

const own_ref =
  OutputReference {
    transaction_id: transaction.placeholder.id,
    output_index: 0,
  }

const own_input = Input { output_reference: own_ref, output: own_utxo }

const base_tx =
  Transaction {
    ..transaction.placeholder,
    inputs: [own_input],
    outputs: [own_output],
    mint: mv,
  }

/// Positive case:
/// Valid state transition succeeds
test test_stateholder_success_valid_state_transition() {
  state_holder.state_holder.spend(
    auth_token: ap,
    datum: Some(initial_state),
    Void,
    own_ref: own_ref,
    transaction: base_tx,
  )
}

/// Negative cases:
/// Condition 0:
/// Fails if more than one output is sent to the script address
test test_stateholder_fail_multiple_outputs_to_own_address() fail {
  let tx = Transaction { ..base_tx, outputs: [own_output, own_output] }

  expect False =
    state_holder.state_holder.spend(
      auth_token: ap,
      datum: Some(initial_state),
      Void,
      own_ref: own_ref,
      transaction: tx,
    )
}

/// Condition 1:
/// Fails if output datum is not an InlineDatum
test test_stateholder_fail_output_without_inline_datum() fail {
  let bad_output = Output { ..own_output, datum: NoDatum }

  let tx = Transaction { ..base_tx, outputs: [bad_output] }

  state_holder.state_holder.spend(
    auth_token: ap,
    datum: Some(initial_state),
    Void,
    own_ref: own_ref,
    transaction: tx,
  )
}

/// Condition 2 & 3:
/// Fails if minting policy or minted token does not match the new state
test test_stateholder_fail_wrong_mint_policy_or_token() fail {
  let wrong_mint = mk_asset(own_hash, adatag, 1)

  let tx = Transaction { ..base_tx, mint: wrong_mint }

  state_holder.state_holder.spend(
    auth_token: ap,
    datum: Some(initial_state),
    Void,
    own_ref: own_ref,
    transaction: tx,
  )
}

/// Condition 4:
/// Fails if output value contains tokens other than the auth token
test test_stateholder_fail_output_contains_extra_tokens() fail {
  let bad_value = ov |> add(other_hash, "x", 1)

  let bad_output = Output { ..own_output, value: bad_value }

  let tx = Transaction { ..base_tx, outputs: [bad_output] }

  state_holder.state_holder.spend(
    auth_token: ap,
    datum: Some(initial_state),
    Void,
    own_ref: own_ref,
    transaction: tx,
  )
}

/// Condition 5:
/// Fails if output contains more than Ada + auth token policies
test test_stateholder_fail_output_with_extra_policy() fail {
  let bad_value = ov |> add(other_hash, "i", 1)

  let bad_output = Output { ..own_output, value: bad_value }

  let tx = Transaction { ..base_tx, outputs: [bad_output] }

  state_holder.state_holder.spend(
    auth_token: ap,
    datum: Some(initial_state),
    Void,
    own_ref: own_ref,
    transaction: tx,
  )
}

/// Condition 6:
/// Fails if operation_count is not incremented
test test_stateholder_fail_operation_count_not_incremented() fail {
  let bad_state =
    State { ..second_state, operation_count: initial_state.operation_count }

  let bad_output = Output { ..own_output, datum: InlineDatum(bad_state) }

  let tx = Transaction { ..base_tx, outputs: [bad_output] }

  state_holder.state_holder.spend(
    auth_token: ap,
    datum: Some(initial_state),
    Void,
    own_ref: own_ref,
    transaction: tx,
  )
}

/// Condition 7:
/// Fails if new tree size is negative
test test_stateholder_fail_negative_tree_size() fail {
  let bad_state = State { ..second_state, size: "-1" }

  let bad_output = Output { ..own_output, datum: InlineDatum(bad_state) }

  let tx = Transaction { ..base_tx, outputs: [bad_output] }

  state_holder.state_holder.spend(
    auth_token: ap,
    datum: Some(initial_state),
    Void,
    own_ref: own_ref,
    transaction: tx,
  )
}

/// Condition 8:
/// Fails if tree size does not change by ±1
test test_stateholder_fail_invalid_tree_size_delta() fail {
  let bad_state = State { ..second_state, size: "5" }

  let bad_output = Output { ..own_output, datum: InlineDatum(bad_state) }

  let tx = Transaction { ..base_tx, outputs: [bad_output] }

  state_holder.state_holder.spend(
    auth_token: ap,
    datum: Some(initial_state),
    Void,
    own_ref: own_ref,
    transaction: tx,
  )
}

/// Condition 9:
/// Fails if minting policy changes between states
test test_stateholder_fail_minting_policy_changed() fail {
  let bad_state = State { ..second_state, minting_policy: own_hash }

  let bad_output = Output { ..own_output, datum: InlineDatum(bad_state) }

  let tx = Transaction { ..base_tx, outputs: [bad_output] }

  state_holder.state_holder.spend(
    auth_token: ap,
    datum: Some(initial_state),
    Void,
    own_ref: own_ref,
    transaction: tx,
  )
}
