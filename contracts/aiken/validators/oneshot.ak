use aiken/collection/list
use cardano/assets.{PolicyId, add, zero}
use cardano/transaction.{Input, NoDatum, Output, OutputReference, Transaction}
use ilap/adatag/tests/fixture.{beneficiary_address, other_hash, own_hash}

/// It does not need any check for correctness and integrity as it can be easily validated
/// when the deployment of the adatag was adversarial or not i.e. not the exact 26 letters were minted.
/// 
/// A very simple one shot minting policy for minting 26 one letter token names that
/// will be used with state holder validator.
validator auth_token(utxo_ref: OutputReference) {
  /// No any check other than the OutRef's is necessary, as the utxo garantees uniquess 
  /// of the authorization token's policy.
  mint(_r: Void, _pid: PolicyId, transaction: Transaction) {
    let Transaction { inputs, .. } = transaction

    let input =
      list.find(inputs, fn(input) { input.output_reference == utxo_ref })
    when input is {
      Some(_) -> True
      None -> False
    }
  }

  else(_) {
    fail
  }
}

/// jq -r ".validators[1].compiledCode"  plutus.json
test test_auth_token() {
  let output =
    Output {
      address: beneficiary_address(),
      value: zero,
      datum: NoDatum,
      reference_script: None,
    }

  let letters =
    [
      "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
      "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
    ]
  let v = zero
  let mv = list.foldl(letters, v, fn(tn, tv) { add(tv, own_hash, tn, 1) })

  // TODO: CHECK TransactionId
  let utxo = OutputReference { transaction_id: #"", output_index: 0 }

  let input = Input { output_reference: utxo, output }

  let tx =
    Transaction {
      ..transaction.placeholder,
      mint: mv,
      extra_signatories: [other_hash],
      inputs: [input],
    }

  //let context = ScriptContext { purpose: Mint(own_hash), transaction: tx }
  // It always succeeds if the OutRef is in hte inputs.
  // goot for initial deployment's for authorization tokens.
  auth_token.mint(utxo, Void, own_hash, tx)
}
