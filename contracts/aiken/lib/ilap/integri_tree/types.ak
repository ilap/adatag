use aiken/hash

// FIXME: blake2b_224 will be used when Plutus V3 is available.
pub type Hash =
  hash.Hash<hash.Sha2_256, ByteArray>

pub type Val {
  xi: ByteArray,
  xa: ByteArray,
  xb: ByteArray,
}

pub type Proof {
  HashNode { hash: Hash, left: Proof, right: Proof }
  NodeHash { hash: Hash }
}
