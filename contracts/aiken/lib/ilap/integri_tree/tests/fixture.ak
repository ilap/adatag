use aiken/builtin
use aiken/math
use aiken/primitive/bytearray
use aiken/primitive/int
use aiken/primitive/string
use ilap/integri_tree/hash.{empty_hash, hash_val, root_hash}
use ilap/integri_tree/types.{HashNode, Node, NodeHash, Proof, Val}

const node_hash = HashNode { hash: empty_hash }

/// Generate a path from a node to the root using empty_hash as other child of a node
pub fn generate_tree(prefix: ByteArray, n: Int, node: Proof, link_at: Int, branch: Proof) -> Proof {
  let e = n % 2 == 1

  let ns = string.to_bytearray(string.from_int(math.pow(2, n / 2)))
  let i = string.to_bytearray(string.from_int(n))
  let i1 = string.to_bytearray(string.from_int(n + 1))

  let a = bytearray.concat(prefix, i)
  let b = bytearray.concat(prefix, i1)

  let v = Val { xi: ns, xa: a, xb: b }
  let vh = hash_val(v)

  when n is {
    0 -> Node { hash: vh, left: node, right: node_hash }
    _ -> {
      //let l = builtin.debug(@"xxxx", generate_tree(prefix, n - 1, node))
      let l = generate_tree(prefix, n - 1, node, link_at, branch)
      let node_hash = when n == link_at is {
        True -> branch
        False -> node_hash
      }
      when e is {
        True -> Node { hash: vh, left: node_hash, right: l }
        False -> Node { hash: vh, left: l, right: node_hash }
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

  let proof1 = generate_tree("ib", n / 2, pan, n / 2, pan)
  let proof = generate_tree("ib", n, pun, n / 2, proof1)

  let root = root_hash(proof)
  let root1 = root_hash(proof1)
  // let nun = Val { xi: idx, xa: "i", xb: "it" }
  (adatag, tree_size, root, root1, un, un, proof, proof1)
}

pub fn generate_valid_proof(
  adatag: ByteArray,
  n: Int,
) -> (ByteArray, ByteArray, NodeHash, NodeHash, Val, Val, Proof, Proof) {
  let ts = math.pow(2, n)
  let tree_size = bytearray.from_string(int.to_string(ts))
  let idx = bytearray.from_string(int.to_string(ts - 1))
  let empty_leaf = HashNode { hash: empty_hash }

  let (low, high) = adatag_bounds(adatag)

  let un = Val { xi: idx, xa: low, xb: high }
  let hun = hash_val(un)
  let pun = Node { hash: hun, left: empty_leaf, right: empty_leaf }

  let an = Val { xi: idx, xa: low, xb: adatag }
  let han = hash_val(an)

  let lv = Val { xi: tree_size, xa: adatag, xb: high }
  let hlv = hash_val(lv)

  let pan =
    Node {
      hash: han,
      left: Node { hash: hlv, left: empty_leaf, right: empty_leaf },
      right: empty_leaf,
    }


  let proof1 = generate_tree(low, n, pan, n, pan)
  let proof = generate_tree(low, n, pun, n, pan)


  let root = root_hash(proof)
  let root1 = root_hash(proof1)

  (adatag, tree_size, root, root1, un, un, proof, proof1)
}

/// Proof for non-membership
/// Constructing the node to update and append
/// Old proof
/// New proof
/// Constructing the Node to update
/// Constructing the node to append
pub fn adatag_bounds(a: ByteArray) -> (ByteArray, ByteArray) {
  let n = bytearray.length(a)
  expect n >= 1 && n <= 13

  (
    if n == 1 {
      let fst = builtin.index_bytearray(a, 0)

      bytearray.push(#"", fst - 1)
    } else {
      bytearray.take(a, n - 1)
    },
    bytearray.concat(a, "a"),
  )
}

test test_adatag_bounds() {
  let (low, high) = adatag_bounds("ilap")
  expect low == "ila" && high == "ilapa"

  let (low1, high1) = adatag_bounds("a")
  expect low1 == "`" && high1 == "aa"

  let (low1, high1) = adatag_bounds("aa")
  expect low1 == "a" && high1 == "aaa"
}
