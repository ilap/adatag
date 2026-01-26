use aiken/primitive/bytearray
//use aiken/builtin
use aiken/primitive/string
use ilap/integri_tree/hash.{empty_hash, hash_val, root_hash}
use ilap/integri_tree/types.{HashNode, Node, NodeHash, Proof, Val}

/// Generate a path from a nood to the root using empty_hash as other child of a node
pub fn generate_tree(prefix: ByteArray, n: Int, node: Proof) -> Proof {
  let e = n % 2 == 1
  let i = string.to_bytearray(string.from_int(n))
  let i1 = string.to_bytearray(string.from_int(n + 1))
  let a = bytearray.concat(prefix, i)
  let b = bytearray.concat(prefix, i1)
  let vh = hash_val(Val { xi: i, xa: a, xb: b })

  when n is {
    0 -> Node { hash: vh, left: node, right: HashNode { hash: empty_hash } }
    _ -> {
      // let l = builtin.debug(@"xxxx", generate_tree(prefix, n - 1, node))
      let l = generate_tree(prefix, n - 1, node)

      when e is {
        True -> Node { hash: vh, left: HashNode { hash: empty_hash }, right: l }
        False ->
          Node { hash: vh, left: l, right: HashNode { hash: empty_hash } }
      }
    }
  }
}

/// Generates a mock proof for labeled tree tests.
pub fn generate_proof(
  n: Int,
) -> (ByteArray, ByteArray, NodeHash, NodeHash, Val, Val, Proof, Proof) {
  let adatag = "ilap"
  let tree_size = "131072"
  let pl = HashNode { hash: empty_hash }

  let idx = "65536"

  let un = Val { xi: idx, xa: "i", xb: "it" }
  let hun = hash_val(un)

  // let un = hash_val(Val { xi: idx, xa: "i", xb: "it" })
  let an = Val { xi: idx, xa: "i", xb: adatag }
  let han = hash_val(an)

  let lv = Val { xi: tree_size, xa: adatag, xb: "it" }
  let hlv = hash_val(lv)

  //let pleaf = Node { hash: hlv, left: pl, right: pl }
  let pun = Node { hash: hun, left: pl, right: pl }

  // let hleaf = combine_three_hashes(hlv, empty_hash, empty_hash)
  let pan =
    Node { hash: han, left: Node { hash: hlv, left: pl, right: pl }, right: pl }

  let proof = generate_tree("ib", n, pun)
  let proof1 = generate_tree("ib", n, pan)
  let root = root_hash(proof)
  let root1 = root_hash(proof1)
  // let nun = Val { xi: idx, xa: "i", xb: "it" }
  (adatag, tree_size, root, root1, un, un, proof, proof1)
}
