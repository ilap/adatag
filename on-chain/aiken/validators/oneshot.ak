use aiken/dict
use aiken/list
use aiken/transaction.{
  Input, Mint, NoDatum, Output, OutputReference, ScriptContext, Transaction,
  TransactionId,
}
use aiken/transaction/value
use ilap/adatag/tests/fixture.{beneficiary_address, other_hash, own_hash}


/// A very simple one shot minting policy for minting 26 one letter token names that
/// will be used with state holder validator.
validator(utxo_ref: OutputReference) {
  /// No any check is necessary, as the utxo garantees uniquess of the authorization token's policy.
  fn auth_token(_d: Void, _r: Void, ctx: ScriptContext) -> Bool {
    let letters =
      [
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
        "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
      ]

    let ScriptContext { transaction, purpose } = ctx
    let Transaction { inputs, mint, .. } = transaction

    expect Mint(policy_id) = purpose
    expect Some(_input) =
      list.find(inputs, fn(input) { input.output_reference == utxo_ref })

    let tokens =
      mint
        |> value.from_minted_value
        |> value.tokens(policy_id)
        |> dict.keys

    tokens == letters
  }
}

/// jq -r ".validators[1].compiledCode"  plutus.json
test test_auth_token() {
  let output =
    Output {
      address: beneficiary_address(),
      value: value.zero(),
      datum: NoDatum,
      reference_script: None,
    }

  let letters =
    [
      "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
      "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
    ]
  let v = value.zero()
  let mv =
    list.foldl(letters, v, fn(tn, tv) { value.add(tv, own_hash, tn, 1) })
      |> value.to_minted_value

  let utxo =
    OutputReference { transaction_id: TransactionId(""), output_index: 0 }

  let input = Input { output_reference: utxo, output }

  let tx =
    Transaction {
      ..transaction.placeholder(),
      mint: mv,
      extra_signatories: [other_hash],
      inputs: [input],
    }
  let context = ScriptContext { purpose: Mint(own_hash), transaction: tx }

  auth_token(utxo, Void, Void, context)
}
