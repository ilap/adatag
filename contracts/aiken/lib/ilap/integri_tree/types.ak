use aiken/crypto.{Blake2b_224, Hash as AikenHash}

pub type Hash =
  AikenHash<Blake2b_224, ByteArray>

pub type Val {
  xi: ByteArray,
  xa: ByteArray,
  xb: ByteArray,
}

pub type Proof {
  HashNode { hash: Hash, left: Proof, right: Proof }
  NodeHash { hash: Hash }
}
