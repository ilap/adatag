use aiken/builtin.{append_bytearray, sha2_256}
use ilap/labeled_tree/types.{Hash, HashNode, NodeHash, Proof, Val}

pub fn combine_hashes(h1: Hash, h2: Hash) -> Hash {
  hash(append_bytearray(h1, h2))
}

pub fn combine_three_hashes(h1: Hash, h2: Hash, h3: Hash) -> Hash {
  hash(append_bytearray(h1, append_bytearray(h2, h3)))
}

pub fn hash_val(val: Val) -> Hash {
  hash(append_bytearray(val.xi, append_bytearray(val.xa, val.xb)))
}

pub fn hash(array: ByteArray) -> Hash {
  sha2_256(array)
}

pub fn root_hash(root: Proof) -> Hash {
  when root is {
    NodeHash { hash } -> hash
    HashNode { hash, left, right } ->
      combine_three_hashes(hash, root_hash(left), root_hash(right))
  }
}
