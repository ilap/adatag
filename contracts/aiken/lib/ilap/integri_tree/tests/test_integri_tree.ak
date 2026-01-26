use ilap/integri_tree/hash.{empty_hash, hash_val, root_hash}
use ilap/integri_tree/proofs.{append_check}
use ilap/integri_tree/tests/fixture.{generate_proof}
use ilap/integri_tree/types.{HashNode, Node, Val}

test test_append_check_2_1() {
  let (adatag, tree_size, root, root1, nun, nun1, proof, _) = generate_proof(2)
  append_check(adatag, tree_size, root, root1, nun, nun1, proof)
}

test test_append_check_2_10() {
  let (adatag, tree_size, root, root1, nun, nun1, proof, _) = generate_proof(20)
  append_check(adatag, tree_size, root, root1, nun, nun1, proof)
}

test test_append_check_2_16() {
  let (adatag, tree_size, root, root1, nun, nun1, proof, _) = generate_proof(32)
  append_check(adatag, tree_size, root, root1, nun, nun1, proof)
}

test test_append_check_2_28() {
  let (adatag, tree_size, root, root1, nun, nun1, proof, _) = generate_proof(56)
  append_check(adatag, tree_size, root, root1, nun, nun1, proof)
}

test test_append_check_2_32() {
  let (adatag, tree_size, root, root1, nun, nun1, proof, _) = generate_proof(64)
  append_check(adatag, tree_size, root, root1, nun, nun1, proof)
}

test test_append_check_2_64() {
  let (adatag, tree_size, root, root1, nun, nun1, proof, _) =
    generate_proof(128)
  append_check(adatag, tree_size, root, root1, nun, nun1, proof)
}

test test_initial_added_state() {
  let adatag = "adam"
  let tree_size = "1"
  let rootVal = Val { xi: "0", xa: "`", xb: "b" }
  let rootVal1 = Val { xi: "0", xa: "`", xb: adatag }
  let nodeVal = Val { xi: tree_size, xa: adatag, xb: "b" }
  let hr = hash_val(rootVal)
  let hr1 = hash_val(rootVal1)
  let hn = hash_val(nodeVal)
  let e = HashNode { hash: empty_hash }

  let tree = Node { hash: hr, left: e, right: e }
  let tree1 =
    Node { hash: hr1, left: Node { hash: hn, left: e, right: e }, right: e }

  append_check(
    adatag,
    tree_size,
    root_hash(tree),
    root_hash(tree1),
    rootVal,
    rootVal,
    tree,
  )
}
