use cardano/assets.{add}
use cardano/transaction.{InlineDatum, NoDatum, Output, Transaction}
use ilap/adatag/tests/fixture.{adversarial_policy, token_policy}
use ilap/adatag/types.{State}
use stateholder
use tests/validator_test_utils.{
  adatag, initial_state, mk_asset, second_state, sov, stateholder_output,
  stateholder_ref, stateholder_tx,
}

/// The initial state must be correct at bootstrap and the valid state 
/// transitions are enforced by the validator scripts. The validator
/// does not check the state proof's correctness only that the state transition
/// is valid according to the rules defined.
///
/// Assumptions for a proper transaction:
/// - spend exactly one input from this script containing the auth token and a valid inline state datum
/// - produce exactly one output to this script containing the auth token and an updated inline state datum
/// - assuming that the validator out_ref parameter points to the input being spent forced by the ledger rules.
/// Positive case:
/// Valid state transition succeeds
test test_stateholder_success_valid_state_transition() {
  stateholder.stateholder.spend(
    token_policy,
    datum: Some(initial_state),
    Void,
    own_ref: stateholder_ref,
    transaction: stateholder_tx,
  )
}

/// Negative cases:
/// Condition 0:
/// Fails if more than one output is sent to the script address
test test_stateholder_fail_multiple_outputs_to_own_address() fail {
  let tx =
    Transaction {
      ..stateholder_tx,
      outputs: [stateholder_output, stateholder_output],
    }

  expect False =
    stateholder.stateholder.spend(
      token_policy,
      datum: Some(initial_state),
      Void,
      own_ref: stateholder_ref,
      transaction: tx,
    )
}

/// Condition 1:
/// Fails if output datum is not an InlineDatum
test test_stateholder_fail_output_without_inline_datum() fail {
  let bad_output = Output { ..stateholder_output, datum: NoDatum }

  let tx = Transaction { ..stateholder_tx, outputs: [bad_output] }

  stateholder.stateholder.spend(
    token_policy,
    datum: Some(initial_state),
    Void,
    own_ref: stateholder_ref,
    transaction: tx,
  )
}

/// Condition 2 & 3:
/// Fails if minting policy or minted token does not match the new state
test test_stateholder_fail_wrong_mint_policy_or_token() fail {
  let wrong_mint = mk_asset(adversarial_policy, adatag, 1)

  let tx = Transaction { ..stateholder_tx, mint: wrong_mint }

  stateholder.stateholder.spend(
    token_policy,
    datum: Some(initial_state),
    Void,
    own_ref: stateholder_ref,
    transaction: tx,
  )
}

/// Condition 4:
/// Fails if output value contains tokens other than the auth token
test test_stateholder_fail_output_contains_extra_tokens() fail {
  let bad_value = sov |> add(token_policy, "x", 1)

  let bad_output = Output { ..stateholder_output, value: bad_value }

  let tx = Transaction { ..stateholder_tx, outputs: [bad_output] }

  stateholder.stateholder.spend(
    token_policy,
    datum: Some(initial_state),
    Void,
    own_ref: stateholder_ref,
    transaction: tx,
  )
}

/// Condition 5:
/// Fails if output contains more than Ada + auth token policies
test test_stateholder_fail_output_with_extra_policy() fail {
  let bad_value = sov |> add(token_policy, "i", 1)

  let bad_output = Output { ..stateholder_output, value: bad_value }

  let tx = Transaction { ..stateholder_tx, outputs: [bad_output] }

  stateholder.stateholder.spend(
    token_policy,
    datum: Some(initial_state),
    Void,
    own_ref: stateholder_ref,
    transaction: tx,
  )
}

/// Condition 6:
/// Fails if operation_count is not incremented
test test_stateholder_fail_operation_count_not_incremented() fail {
  let bad_state =
    State { ..second_state, operation_count: initial_state.operation_count }

  let bad_output =
    Output { ..stateholder_output, datum: InlineDatum(bad_state) }

  let tx = Transaction { ..stateholder_tx, outputs: [bad_output] }

  stateholder.stateholder.spend(
    token_policy,
    datum: Some(initial_state),
    Void,
    own_ref: stateholder_ref,
    transaction: tx,
  )
}

/// Condition 7:
/// Fails if new tree size is negative
test test_stateholder_fail_negative_tree_size() fail {
  let bad_state = State { ..second_state, size: "-1" }

  let bad_output =
    Output { ..stateholder_output, datum: InlineDatum(bad_state) }

  let tx = Transaction { ..stateholder_tx, outputs: [bad_output] }

  stateholder.stateholder.spend(
    token_policy,
    datum: Some(initial_state),
    Void,
    own_ref: stateholder_ref,
    transaction: tx,
  )
}

/// Condition 8:
/// Fails if tree size does not change by ±1
test test_stateholder_fail_invalid_tree_size_delta() fail {
  let bad_state = State { ..second_state, size: "5" }

  let bad_output =
    Output { ..stateholder_output, datum: InlineDatum(bad_state) }

  let tx = Transaction { ..stateholder_tx, outputs: [bad_output] }

  stateholder.stateholder.spend(
    token_policy,
    datum: Some(initial_state),
    Void,
    own_ref: stateholder_ref,
    transaction: tx,
  )
}

/// Condition 9:
/// Fails if minting policy changes between states
test test_stateholder_fail_minting_policy_changed() fail {
  let bad_state = State { ..second_state, minting_policy: adversarial_policy }

  let bad_output =
    Output { ..stateholder_output, datum: InlineDatum(bad_state) }

  let tx = Transaction { ..stateholder_tx, outputs: [bad_output] }

  stateholder.stateholder.spend(
    token_policy,
    datum: Some(initial_state),
    Void,
    own_ref: stateholder_ref,
    transaction: tx,
  )
}
