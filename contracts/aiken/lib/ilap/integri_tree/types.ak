use aiken/crypto

// FIXME: blake2b_224 will be used when Plutus V3 is available.
pub type Hash =
  crypto.Hash<crypto.Blake2b_224, ByteArray>

pub type Val {
  xi: ByteArray,
  xa: ByteArray,
  xb: ByteArray,
}

pub type Proof {
  HashNode { hash: Hash, left: Proof, right: Proof }
  NodeHash { hash: Hash }
}
