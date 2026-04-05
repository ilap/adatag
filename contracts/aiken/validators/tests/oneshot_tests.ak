use aiken/collection/list
use cardano/assets
use cardano/transaction.{NoDatum, Transaction}
use ilap/adatag/tests/fixture.{beneficiary_address, token_policy}
use oneshot
use tests/validator_test_utils.{mk_input, mk_output, mk_outref}

const letters =
  [
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
    "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
  ]

const output = mk_output(beneficiary_address, assets.zero, NoDatum)

const mint_value =
  list.foldl(
    letters,
    assets.zero,
    fn(tn, acc) { assets.add(acc, token_policy, tn, 1) },
  )

const outref = mk_outref(0)

const valid_tx =
  Transaction {
    ..transaction.placeholder,
    mint: mint_value,
    inputs: [mk_input(0, output)],
  }

/// jq -r ".validators[1].compiledCode"  plutus.json
test test_auth_token_outref_in_tx_passes() {
  oneshot.oneshot.mint(outref, Void, Void, valid_tx)
}

test test_auth_token_outref_not_in_tx_fails() fail {
  // A bad tx that does not include the required UTxO in its inputs
  let bad_tx = Transaction { ..valid_tx, inputs: [mk_input(1, output)] }

  oneshot.oneshot.mint(outref, Void, Void, bad_tx)
}
