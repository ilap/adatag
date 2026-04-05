use aiken/collection/list
use cardano/transaction.{Input, OutputReference, Transaction}

/// It does not need any check for correctness and integrity as it can be 
/// easily validated when the deployment of the adatag was adversarial or not
/// i.e. not the exact 26 letters were minted.
/// 
/// A very simple one shot minting policy for minting 26 one letter token names
/// that will be used with state holder validator.
validator oneshot(utxo_ref: OutputReference) {
  /// No any check other than the OutRef's is necessary, as the utxo spent
  /// garantees uniqueness of the authorization token's policy.
  mint(_r: Void, _pid: Void, tx: Transaction) {
    let Transaction { inputs, .. } = tx
    // Only transaction `inputs` can be spent; `reference_inputs` are
    // read-only UTxOs.
    let input = list.find(inputs, fn(i) { i.output_reference == utxo_ref })
    when input is {
      Some(_) -> True
      None -> False
    }
  }

  else(_) {
    fail @"unsupported purpose"
  }
}
