use aiken/hash

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
