use aiken/bytearray
use ilap/integri_tree/hash.{combine_three_hashes, empty_hash, hash_val}
use ilap/integri_tree/types.{Hash, HashNode, NodeHash, Proof, Val}

///
/// The `delete_values` function creates the required 2 updated values from the nodes that will be 
/// modified (updated and deleted) in the tree during the deletion operation.
///
/// The `delete_values` function is called with the following arguments:
/// - `x`: the input `ByteArray` representing the element to be deleted.
/// - `nuv1`: Val of the update node in the tree before deletion.
/// - `nuv2`: Val of the other update node in the tree before deletion.
/// - `ndv`: Val of the node to be deleted (last node of the tree).
///
/// The function returns a tuple containing the updated update nodes required
/// for proving the deletion.
///
/// Example usage:
/// ```aiken
/// let updated_vals = delete_values(x: e, nuv1: nuv1, nuv2: nuv2, ndv: ndv)
/// ```
/// Note: Validate the following scenarious:
/// 1
fn delete_values(x: ByteArray, nuv1: Val, nuv2: Val, ndv: Val) -> (Val, Val) {
  // Condition 1. The first update node must contain `(xa, x)` and the second `(x, xb)` 
  if
  bytearray.compare(nuv1.xb, x) == Equal && bytearray.compare(nuv2.xa, x) == Equal{
  
    // Condition 2. the first updated node must contain `(xa, nuv2.xb)`
    let u1 = Val { xi: nuv1.xi, xa: nuv1.xa, xb: nuv2.xb }
    // Condition 3. If nuv1 is not equal to nuv2 than the second updated node must contain `(nd.xa, nd.xb)`
    // otherwise the second update node is equal to the updated node.
    let u2 =
      if nuv1 == nuv2 {
        u1
      } else {
        Val { xi: nuv2.xi, xa: ndv.xa, xb: ndv.xb }
      }

    (u1, u2)
  } else {
    fail
  }
}

///
/// Brief: Function to check the correctness of the deletion proof
///
fn check_delete_proof(
  is_new_state: Bool,
  root: Hash,
  proof: Proof,
  nuh1: Hash,
  nuh2: Hash,
  ndh: Hash,
  nodes: (Val, Val),
) -> Bool {
  let (has_nuh1, has_nuh2, has_ndh, calculated_root) =
    validate_delete_proof(
      is_new_state: is_new_state,
      has_nu1: False,
      has_nu2: False,
      has_nd: False,
      node: proof,
      nuh1: nuh1,
      nuh2: nuh2,
      ndh: ndh,
      nodes: nodes,
    )
  has_nuh1 && has_nuh2 && has_ndh && calculated_root == root
}

pub fn validate_delete_proof(
  is_new_state: Bool,
  has_nu1: Bool,
  has_nu2: Bool,
  has_nd: Bool,
  node: Proof,
  nuh1: Hash,
  nuh2: Hash,
  ndh: Hash,
  nodes: (Val, Val),
) -> (Bool, Bool, Bool, Hash) {
  when node is {
    NodeHash { hash } -> (has_nu1, has_nu2, has_nd, hash)
    HashNode { hash, left, right } -> {
      let enu1 = hash == nuh1
      let enu2 = hash == nuh2
      let end = hash == ndh

      let has_nu1 = has_nu1 || enu1
      let has_nu2 = has_nu2 || enu2
      let has_nd = has_nd || end

      let (lnu1, lnu2, lnd, lh) =
        validate_delete_proof(
          is_new_state: is_new_state,
          has_nu1: has_nu1,
          has_nu2: has_nu2,
          has_nd: has_nd,
          node: left,
          nuh1: nuh1,
          nuh2: nuh2,
          ndh: ndh,
          nodes: nodes,
        )

      let (rnu1, rnu2, rnd, rh) =
        validate_delete_proof(
          is_new_state: is_new_state,
          has_nu1: has_nu1,
          has_nu2: has_nu2,
          has_nd: has_nd,
          node: right,
          nuh1: nuh1,
          nuh2: nuh2,
          ndh: ndh,
          nodes: nodes,
        )

      // When the new state is being validated and one of the amendable nodes is found,
      // the updated state's hash is calculated. Otherwise, the normal hash of the proof's node 
      // is calculated.
      if is_new_state && ( enu1 || enu2 || end ) {
        // The new state 
        //let (nuv1, nuv2) = nodes

        //let (nvh, nlh, nrh) =
        // Scenarious:
        // 1. nuv1 == nuv2: 
        // 1. nuv1 == nuv2 && nuv1 != end -> Ok for further validation
        // 2. nuv1 == nuv2 && nuv2 == end -> Fail
        // 3. nuv1 != nuv2 && nuv1 == end -> Fail
        // 4. nuv1 != nuv2 && nuv2 == end -> Fail
        // The end must be always differant than nuv1 && nuv2
        //  if nuv1 == nuv2 && nuvend {
        //let leaf_h =
        //  combine_three_hashes(hash_val(leaf), empty_hash, empty_hash)
        //      let (al, ar) =
        // The only valid combination to ensure the integrity of the tree are:
        // 1. left and right are emtpy hashes
        // 2. left is not empty and right is empty
        // otherwise invalid.
        //        if lh == empty_hash && rh == empty_hash {
        //          (empty_hash, rh)
        //        } else if lh != empty_hash && rh == empty_hash {
        //          (lh, empty_hash)
        //        } else {
        // The right must always be an empty hash constraints above are not met
        //          fail
        //        }
        //      (hash_val(nuv1), al, ar)
        //    } else if end {
        // Only the val is changed
        //      (hash_val(nuv1), lh, rh)
        //    } else {
        // Impossible happened
        //      fail
        //    }

        //  (lnu1 || rnu1, lnu2 || rnu2, lnd || rnd, combine_three_hashes(nvh, nlh, nrh))
        (True, True, True, empty_hash)
      } else {
        (
          lnu1 || rnu1,
          lnu2 || rnu2,
          lnd || rnd,
          combine_three_hashes(hash, lh, rh),
        )
      }
    }
  }
}

pub fn delete_check(
  e: ByteArray,
  root: Hash,
  _updated_root: Hash,
  nuv1: Val,
  nuv2: Val,
  ndv: Val,
  proof: Proof,
) -> Bool {
  let nuh1 = hash_val(nuv1)
  let nuh2 = hash_val(nuv2)
  let ndh = hash_val(ndv)

  let nodes = delete_values(x: e, nuv1: nuv1, nuv2: nuv2, ndv: ndv)
  // Check the root hash before and after the insertion of the element.
  check_delete_proof(False, root, proof, nuh1, nuh2, ndh, nodes)
  //&& check_delete_proof(True, updated_root, proof, nuh1, nuh2, ndh, nodes,)
}

pub fn delete_check2(
  e: ByteArray,
  root: Hash,
  updated_root: Hash,
  nuv1: Val,
  nuv2: Val,
  ndv: Val,
  proof: Proof,
) -> Bool {
  let nuh1 = hash_val(nuv1)
  let nuh2 = hash_val(nuv2)
  let ndh = hash_val(ndv)

  let nodes = delete_values(x: e, nuv1: nuv1, nuv2: nuv2, ndv: ndv)
  // Check the root hash before and after the insertion of the element.
  check_delete_proof(False, root, proof, nuh1, nuh2, ndh, nodes)? && check_delete_proof(
    True,
    updated_root,
    proof,
    nuh1,
    nuh2,
    ndh,
    nodes,
  )?
}

test test_delete_state() {
  let adatag = "adam"
  let tree_size = "1"
  let rootVal = Val { xi: "0", xa: "`", xb: "b" }
  let nuv1 = Val { xi: "0", xa: "`", xb: adatag }
  let nuv2 = Val { xi: tree_size, xa: adatag, xb: "b" }
  let hr = hash_val(rootVal)
  let hr1 = hash_val(nuv1)
  let hn = hash_val(nuv2)
  let e = NodeHash { hash: empty_hash }

  let tree = HashNode { hash: hr, left: e, right: e }
  let tree1 =
    HashNode {
      hash: hr1,
      left: HashNode { hash: hn, left: e, right: e },
      right: e,
    }
  // FIXME: Implement delete/burning
  tree != tree1
  // delete_check2(
  //  adatag,
  //  root_hash(tree1),
  //  root_hash(tree),
  //  nuv1,
  //  nuv2,
  //  nuv2,
  //  tree1,
  //)
}
