use aiken/hash

pub type Hash = hash.Hash<hash.Blake2b_224, ByteArray>

pub type Val {
  xi: ByteArray,
  xa: ByteArray,
  xb: ByteArray,
}

pub type Proof {
  NodeHash { hash: Hash }
  HashNode { hash: Hash, left: Proof, right: Proof }
}