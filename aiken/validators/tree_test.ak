// use aiken/transaction.{OutputReference, ScriptContext, Spend, TransactionId}
// use ilap/labeled_tree/hash.{hash_val, root_hash}
use ilap/labeled_tree/proofs.{append_check}
use ilap/labeled_tree/types.{
  Hash, Proof, Val,
}
use ilap/labeled_tree/utils.{generate_proof}

type TreeRedeemer {
  adatag: ByteArray,
  tree_size: ByteArray,
  old_state: Hash,
  new_state: Hash,
  update_val: Val,
  append_val: Val,
  proof: Proof,
}

validator {
  fn labeled_tree(_d: Void, rdmr: TreeRedeemer, _ctx: Void) -> Bool {
    append_check( rdmr.adatag, rdmr.tree_size, rdmr.old_state, rdmr.new_state, rdmr.update_val, rdmr.append_val, rdmr.proof,)
  }
}


test test_labeled_tree() {
  let (adatag, tree_size, root, root1, nun, nun1, proof, _) =  generate_proof(64)

  let r =
    TreeRedeemer {
      adatag,
      tree_size,
      old_state: root,
      new_state: root1,
      update_val: nun,
      append_val: nun1,
      proof,
    }
  // 
  // let placeholder_utxo =
  //  OutputReference { transaction_id: TransactionId(""), output_index: 0 }
  // let context =
  //  ScriptContext {
  //    purpose: Spend(placeholder_utxo),
  //    transaction: transaction.placeholder(),
  //  }
  // cbor.diagnostic(root) == @"a"
  //root == hash("65536iit")
  //root == hash("65536iilap")
  // cbor.diagnostic(proof1) == @"a"
  // root == root1
  //root == hash_val(Val { xi: "65536", xa: "i", xb: "ilap" })
  //root == hash_val(Val { xi: "65536", xa: "i", xb: "it" })
  labeled_tree(Void, Void, rdmr: r)
}
