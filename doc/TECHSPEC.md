# IntegriTree: Efficient Proofs for Dynamic Sets

Verifying large sets of elements efficiently can be challenging. Traditional hash trees offer security but lack efficient updates.

IntegriTree, a new accumulator scheme, leverages a complete binary hash tree for compact accumulation and dynamic updates which relies on a trusted Plutus script, specifically designed to perform accumulator verification.

## Key features:

- Unlike traditional hash trees, IntegriTree stores values in every node.
- These values are represented as (open-interval) pairs of - consecutive elements with each node in the tree.
- Offers proofs for membership, non-membership, addition, and deletion.
- Proof size scales logarithmically with the number of elements.

> Example: Verifying the presence/absence of an element `x` in the tree `T` as a model of accumulated set `S = {x1, ..., xn}`, one must prove that a pair `(xα, xβ)`, where `xα ≺ x ≺ xβ` (absence), or either of the pairs `(xα, x)` and `(x, xβ)` (presence), are present in the `T` tree.

In addition, verifying addition of an element `x` requires proving the existence of `(xα, xβ)`, where `xα ≺ x ≺ xβ` of the tree `T` to be appended, and ensuring that the pairs `(xα, x)` and `(x, xβ)`, are present in the new updated tree `T'` while maintaining completeness. Deletion from the `T` requires similar proofs.

> Note: Open-interval pairs enforce the uniqueness of the elements by setting the initial value to some minimum and maximum bounds. For example `(xα: -Infinity, xβ: +Infinity)`.

In our `@adatag` Proof of Concept (PoC) implementation, the lower and upper bounds will be represented by lowercase letters (in lexicographical order), for example the `xα: "a"` and `xβ: "c"`, with the additional constraint that the first letter of the element must be `"b"`. This ensures that only elements starting with `"b"` are included in the interval. However, in other implementations, they could take any form that enforces the condition `xα ≺ x ≺ xβ`, such as hashes (ranging from `"0x0000...00"` to `"0xFFFF...FF"`).

## Brief Overview of Proofs

The accumulator `accT` of the tree `T` is the `hashRoot(T)`, calculated by combining the hashes of the node's value, the hashRoot of its left child, and the hashRoot of its right child: `accN = hash( hashVal(value) || hashRoot(leftChild) || hashRoot(rightChild))`, where `||` represents the concatenation of byte arrays.

> Note: Any node `N` of the tree `T` can be accumulated in the same way as above.

The proof provided by IntegriTree consists of a compact path (minimal subtree) from the root node `Nr` to the provable node(s), ensuring that the root hash of this subtree matches the root hash of the entire tree `T`, and at least one (depends on the proof type) node's value of the provable node is included in the proof.

For example, a complex addition proof involves creating a minimal subtree from two nodes:

1. The node with value `(xα, xβ)` called `Vu`, where `xα ≺ x ≺ xβ`, proving that the element `x` does not exist in the tree.
2. The parent node `Np`, which contains the value `Vp`, the new leaf will be appended to.

> Note: to enforce completness of the tree the nodes' value contains the index `xi` that represents the size of the tree in the current state. Therefore, the appendable node's index `xi` is always half (by integer deivision) of the new size of the tree (`old_state.tree_size +/- 1` depends on the operation)

During validation, the Plutus script checks the following:

- The accumulator in the old state matches with the calculated root hash from the provided proof (minimal subtree), and the two values (`Vu` and `Vp`) provided by the users.
- The Plutus script generates three new values (`Vu'`, `Vp'` and `Vl`) from the two provided values and the element `x` to be added into the tree by:
  - Replacing the `Vu`'s `(xα, xβ)` with `(xα, x)` and
  - Replacing the `Vp`'s values if it's required (see details in the implemented proofs)
  - Creating a new leaf value `Vl` with `(x, xβ)`.
- Finally, the accumulator of the new state matches with calculated in the plutus script from the original proof, and where the two values (`Vu` and `Vp`) are replaced by the three new values during the validation process.

During the validation the value where:

1. the current node's `valHash == hashVal(Vu)`, the `hashVal` will be replaced by `Vu'` and where
2. the current nodes's `valHash == hashVal(Vp)`, the `hashVal` will be replaced with `Vp'` and one of its children will be replaced by the `hash( hashVal(Vl) || empty_hash || empty_hash)`

> Note: which children is replaced is depends on the new tree size (even-odd rule)

## Implementation

This scheme is implemented within the Cardano blockchain system. The blockchain will store only the states of the tree T, which include:

- The accumulator (root hash) of the tree T
- The operation performed (added/deleted)
- The current size of the tree and
- The element added or deleted.

The state change will be bound to and carried by an authorization token held at the relevant script address.

Each state change (element added or removed) is validated by the corresponding Plutus minting script (mint/burn). This validation process is based on the old state, the user's input as redeemer, and the provided new state by the user(s) as an inline datum of the Extended UTXO (EUTxO) containing the authorization token.

> Note: The redeemer contains the proof(s) and the required values (`Val`s) are required for minting and burning elements.

Therefore, the validation process utilizes these three parts:

- The existing old state stored on chain as an EUTxO.
- The user's provided new state as an output.
- The append or delete proof and the required Values as the user's input.

Indeed, the implementation resembles a state machine, tracking transitions from an initial state (represented by the root hash of the empty tree, which contains the initial lower and upper bounds) to various states as operations are performed on the tree.

> Note: Users can reconstruct the whole tree to any of these states using the specified transparent off-chain append and delete functions. Each state transition is validated by the blockchain system, ensuring the integrity and correctness of the tree's structure throughout its evolution. This approach provides a reliable and transparent mechanism for managing dynamic sets of elements on the Cardano blockchain.

## IntegriTree vs. Minimal Subtree

- **IntegriTree Data Structure**: Represents the complete structure of the tree, including Val, left and right child nodes.
- **Minimal Subtree**: Used for proofs and contains only hash values of vals, nodes and branches along the path of specific nodes.

## Node Structure

Each node in the IntegriTree consists of:

- `Val`: A data structure containing information about the node, including the index of the node in the tree in level-order i.e. size of the tree (`xi`) and the pairs of consecutive elements (as two bounds) of the open-interval (`xa` and `xb`).
- `Children`: left and right child nodes.

Typescript's example:

```typescript
export type Val = { xi: string; xa: string; xb: string }

export type IntegriTree = {
  val: Val
  left: IntegriTree
  right: IntegriTree
}
```

> Note: the current typescript implementation of IntegriTree uses an array of Vals for efficient operations.

### Root Hash Calculation of IntegriTree

The off-chain root hash calculateion of the tree or any of its branch is calculated using the following function:

```typescript

  /**
   * Calculates the hash for a specified node in the tree.
   * @param index - The index of the node.
   * @returns The hash for the specified node.
   */
  private hashNode(index: number): string {
    if (index >= this.elements.length) {
      return emptyHash
    }

    const val = this.elements[index]
    const leftChildIndex = 2 * index + 1
    const rightChildIndex = 2 * index + 2

    const valHash = hashVal(val)
    const leftHash = this.hashNode(leftChildIndex)
    const rightHash = this.hashNode(rightChildIndex)

    return combineThreeHashes(valHash, leftHash, rightHash)
  }
```

> Note: A compact array based structure is used for off-chain representation of the tree.

## Minimal Subtree (Proof)

The proof provided by IntegriTree is a minimal subtree together with the required values (depends on the proofs). In other words, the minimal subtre is a path from the root node to the provable node (membership or non-membership) contains hash of the nodes' value the hash of the child not in the path down to the node and the child contains the node as path down to the provabel node.

> Note: The minimal subtree is used for proofs and contains only hash values of Vals, nodes and branches along the path of specific nodes (e.g., membership, non-membership, update, delete).

Typescript definition example of the minimal subtree (TreeProof)

```typescript
export type TreeProof =
  | {
      // `hash` is the valHash of the Val of the node
      HashNode: { hash: string; left: TreeProof; right: TreeProof }
    }
  | {
      // `hash` is the rootHash of a node, a branch etc.
      NodeHash: { hash: string }
    }
```

This structure regenerates the exact hash of the tree as the whole IntegriTree would be hashed.

In merkle tree the leaf pairs (that contain data) are hashed up to the tree while in IntegriTree the nodes' values (`Val`) are part of the proof.
For example, for the following Proof

```typescript
const proof = HashNode {
  hash: valHash(Val{ xi: 1, xa: "a", xb: "adam"}),
  left: HashNode {
            hash: valHash(Val{ xi: 2, xa: "adam", xb: "b"}),
            left: emptyHash,
            right: emptyHash
        },
  right: emptyHash
}
```

It's easily can be proven that "adam" is in the tree, but "aby" is not, having a `Val { xi: "1", xa: "a", xb: "adam"}` and the Proof
as the proof's `rootHash(proof)` exactly the same with the rootHash of the whole integritree, and the hash of proof contains the hash of the required proovable `Val`

### Root Hash Calculation of the minimal subtree

The root hash of the minimal subtree use similar function as in the IntegriTree, see the `aiken`'s implementation below:

```gleam
pub fn root_hash(root: Proof) -> Hash {
  when root is {
    NodeHash { hash } -> hash
    HashNode { hash, left, right } ->
      combine_three_hashes(hash, root_hash(left), root_hash(right))
  }
}
```

## Membership and Non-Membership Proofs

IntegriTree provides proofs for both membership and non-membership of elements in the tree:

- **Membership**: If an `x` element is in the tree, there exists at least one node where either `xa = x` or `xb = x`.
- **Non-Membership**: If an element is not in the tree, there exists a node where `xa < x < xb`.

## Append and Delete Proofs

Proofs are available for appending new elements to the tree and deleting existing elements:

- **Append Proof**: If an element is not in the tree, the proof contains the non-membership (update node's) value `Vu`, the value `Vp` of the node will be the parent of the new appendable node of the new element, and the minimal subtree of the two `Val`'s nodes.
- **Delete Proof**: If an element is in the tree, the proof contains the two membership (update nodes') values the `Vu1` and `Vu2` (the `(xα, x)` and `(x, xβ)`), and the value `Vp` the parent node of the last (in level-order) node in the tree, and the minimal subtree create from the nodes of the three values.

### Val Types of the Proof

There are two types of Vals are used with the Proof:

- **Updateable Node's (`Nu`'s)**: Nodes' val that are already part of the tree but may have their values updated.
- **Parent Node's (`Np`'s)**: Nodes' val where a new leaf will be appended to or removed from.

## Completeness Enforcement

The tree must always maintain its completeness which enforced by the sequential index of nodes in the tree.

## Technical briefs of the IntegriTree

- The tree model **`T`** representing the set **`X = {x1, ..., xn}`** is and must always be a **`Complete Binary Tree`**.
- A `Complete Binary Tree` can have an incomplete last level, as long as all the leaves in that level are arranged from left to right.
- A **node** (**`N`**) of the `T` tree has two pointers: one to its left child, and one to its right child.
- A **leaf** (**`L`**) is a node that has two empty hashes (**`ε, ε`**) as its children.
- Every node in `T` must be assigned a sequential index **`i`** starting from **`1`**, with the root of T having index 1, and this index `i` increases by 1 whenever a new node **`Ni`** is added. The value of `i` is always equal to the number of nodes (and not the numb er of elements) in `T`.
- Therefore, the nr. of elements of the tree is always the `tree size - 1`
- The depth (**`d`**) is defined as the length of the simple path (number of edges) from the `root` node of `T` to node `N`.
- The node's index (`i`) must always be exactly double of its parent's index if it's the left child, or 2 times of the parent's index plus 1 if it's the right child.
- The integrity of `T` is ensured by the sequential index (`i`).
- Therefore the node index determines the position of any node within the 'T' tree and positions in its parent, except for the root node (as it doesn't have a parent).
  - The position of a node in the tree is determined by **`P = i - 2^d + 1`** which means it's the **`Pth`** node on level `d` of the `T` tree.
  - The **left**/**right** position within a node follows the **`even-odd`** rule: if its index `i` is even, the child is on the left side; otherwise, it's on the right. This position is determined by **`Pn = In - 2Ip`** (`Ip` is the parent's index) where `Pn` is 0 when the child is on the left, 1 when it's on the right, and results in an **error** otherwise.
- In order to keep track of changes in the state of the `T` tree, there are two types of nodes:
  - Updateable node (**`Nu`**): These nodes are already part of the tree, but their values will be updated sometime by an update process.
  - Parent node (**`Np`**): These are then nodes where a new leaf `L` will be appended to or the last leaf removed from.
- The right child (`Nr`) of an parent node (`Np`), must always be an empty hash, represented as **`ε`**. The left child can be either an empty hash (**`ε`**) or a leaf.

## Appendix

### Letter Frequency Table for English Words

| Letter | Approximate Frequency |
| ------ | --------------------- |
| E      | ~12-15%               |
| T      | ~8-10%                |
| A      | ~7-9%                 |
| O      | ~7-8%                 |
| I      | ~6-7%                 |
| N      | ~6-7%                 |
| S      | ~6-7%                 |
| H      | ~5-6%                 |
| R      | ~5-6%                 |
| D      | ~4-5%                 |
| L      | ~3-4%                 |
| U      | ~2-3%                 |
| M      | ~2-3%                 |
| W      | ~2-3%                 |
| C      | ~2-3%                 |
| F      | ~2%                   |
| G      | ~1-2%                 |
| Y      | ~1-2%                 |
| P      | ~1-2%                 |
| B      | ~1-2%                 |
| V      | ~1%                   |
| K      | ~0.5-1%               |
| J      | ~0.1-0.5%             |
| X      | ~0.1-0.5%             |
| Q      | ~0.1%                 |
| Z      | ~0.1%                 |
