use aiken/crypto

// Blake2b_224 is used as Plutus V3 is available now.
pub type NodeHash =
  crypto.Hash<crypto.Blake2b_224, ByteArray>

pub type Val {
  xi: ByteArray,
  xa: ByteArray,
  xb: ByteArray,
}

pub type Proof {
  Node { hash: NodeHash, left: Proof, right: Proof }
  HashNode { hash: NodeHash }
}
