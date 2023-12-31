use ilap/labeled_tree/proofs.{append_check}
use ilap/labeled_tree/tests/fixture.{generate_proof}

//use aiken/cbor

test test_append_check_2_1() {
  let (adatag, tree_size, root, root1, nun, nun1, proof, _) = generate_proof(2)
  //cbor.diagnostic(proof) == @"a"
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
