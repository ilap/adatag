use aiken/builtin.{append_bytearray, blake2b_224}
use ilap/integri_tree/types.{HashNode, Node, NodeHash, Proof, Val}

//use aiken/cbor
//
// Blake2b_224
pub const empty_hash =
  #[
    0x83,
    0x6c,
    0xc6,
    0x89,
    0x31,
    0xc2,
    0xe4,
    0xe3,
    0xe8,
    0x38,
    0x60,
    0x2e,
    0xca,
    0x19,
    0x02,
    0x59,
    0x1d,
    0x21,
    0x68,
    0x37,
    0xba,
    0xfd,
    0xdf,
    0xe6,
    0xf0,
    0xc8,
    0xcb,
    0x07,
  ]

// sha2_256
// pub const empty_hash =
//  #[
//    0xe3,
//    0xb0,
//    0xc4,
//    0x42,
//    0x98,
//   0xfc,
// 0x1c,
//  0x14,
//  0x9a,
//  0xfb,
//  0xf4,
//  0xc8,
//  0x99,
//  0x6f,
//  0xb9,
//  0x24,
//  0x27,
// 0xae,
// 0x41,
// 0xe4,
// 0x64,
// 0x9b,
//  0x93,
//  0xa4,
//0x95,
//  0x99,
//    0x1b,
//  0x78,
// 0x52,
//   0xb8,
//    0x55,
//  ]

/// 0x4c,
pub fn combine_hashes(h1: NodeHash, h2: NodeHash) -> NodeHash {
  hash(append_bytearray(h1, h2))
}

pub fn combine_three_hashes(
  h1: NodeHash,
  h2: NodeHash,
  h3: NodeHash,
) -> NodeHash {
  hash(append_bytearray(h1, append_bytearray(h2, h3)))
}

pub fn hash_val(val: Val) -> NodeHash {
  hash(append_bytearray(val.xi, append_bytearray(val.xa, val.xb)))
}

pub fn hash(array: ByteArray) -> NodeHash {
  // FOXME: switched to Blake2b_224
  //sha2_256(array)
  blake2b_224(array)
}

pub fn root_hash(root: Proof) -> NodeHash {
  when root is {
    HashNode { hash } -> hash
    Node { hash, left, right } ->
      combine_three_hashes(hash, root_hash(left), root_hash(right))
  }
}
