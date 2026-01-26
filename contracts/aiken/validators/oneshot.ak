use aiken/collection/list
use cardano/assets.{PolicyId}
use cardano/script_context.{ScriptContext}
use cardano/transaction.{Input, NoDatum, Output, OutputReference, Transaction}
use ilap/adatag/tests/fixture.{beneficiary_address, other_hash, own_hash}

/// It does not need any check for correctness and integrity as it can be easily validated
/// when the deployment of the adatag was adversarial or not i.e. not the exact 26 letters were minted.
/// 
/// A very simple one shot minting policy for minting 26 one letter token names that
/// will be used with state holder validator.
validator auth_token(utxo_ref: OutputReference) {
  /// No any check other than the OutRef's is necessary, as the utxo garantees uniqueness
  /// of the authorization token's policy.
  mint(_r: Void, _pid: PolicyId, tx: Transaction) {
    let Transaction { inputs, .. } = tx

    let input =
      list.find(inputs, fn(input) { input.output_reference == utxo_ref })
    when input is {
      Some(_) -> True
      None -> False
    }
  }

  else(_ctx: ScriptContext) {
    fail @"unsupported purpose"
  }
}

/// jq -r ".validators[1].compiledCode"  plutus.json
test test_auth_token() {
  let output =
    Output {
      address: beneficiary_address(),
      value: assets.zero,
      datum: NoDatum,
      reference_script: None,
    }

  let letters =
    [
      "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
      "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
    ]
  let v = assets.zero
  let mv =
    list.foldl(letters, v, fn(tn, tv) { assets.add(tv, own_hash, tn, 1) })

  let utxo =
    OutputReference {
      transaction_id: transaction.placeholder.id,
      output_index: 0,
    }

  let input = Input { output_reference: utxo, output }

  let tx =
    Transaction {
      ..transaction.placeholder,
      mint: mv,
      extra_signatories: [other_hash],
      inputs: [input],
    }

  // It always succeeds if the OutRef is in the inputs.
  // good for initial deployment's for authorization tokens.
  auth_token.mint(
    utxo,
    Void,
    #"02030405060708091011121314150203040506070809101112131415",
    tx,
  )
}
