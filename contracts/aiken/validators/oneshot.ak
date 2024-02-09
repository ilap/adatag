use aiken/list
use aiken/transaction.{
  Input, Mint, NoDatum, Output, OutputReference, ScriptContext, Transaction,
  TransactionId,
}
use aiken/transaction/value
use ilap/adatag/tests/fixture.{beneficiary_address, other_hash, own_hash}

/// It does not need any check for correctness and integrity as it can be easily validated
/// when the deployment of the adatag was adversarial or not i.e. not the exact 26 letters were minted.
/// 
/// A very simple one shot minting policy for minting 26 one letter token names that
/// will be used with state holder validator.
validator(utxo_ref: OutputReference) {
  /// No any check other than the OutRef's is necessary, as the utxo garantees uniquess 
  /// of the authorization token's policy.
  fn auth_token(_r: Void, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    let Transaction { inputs, .. } = transaction

    expect Mint(_policy_id) = purpose
    let input =
      list.find(inputs, fn(input) { input.output_reference == utxo_ref })
    when input is {
      Some(_) -> True
      None -> False
    }
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

  // It always succeeds if the OutRef is in hte inputs.
  // goot for initial deployment's for authorization tokens.
  auth_token(utxo, Void, context)
}
