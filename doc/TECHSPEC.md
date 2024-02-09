# IntegriTree Technical Specifications

## Introduction

IntegriTree is a complete binary tree structure designed to ensure the integrity and uniqueness of elements (using open-interval) within a decentralized environment. This document presents a detailed technical specification of IntegriTree.


## Node Structure

Each node in the IntegriTree consists of:

- `Val`: A data structure containing information about the node, including the size of the tree (`xs`) and the two bounds of the open-interval (`xa` and `xb`).
- `Children`: left and right child nodes.

Typescript's example:
``` typescript

export type Val =  { xs: string; xa: string; xb: string }

export type IntegriTree = {
  val: Val;
  left: IntegriTree;
  right: IntegriTree;
}

```

## Membership and Non-Membership Proofs

IntegriTree provides proofs for both membership and non-membership of elements in the tree:

- **Membership**: If an element is in the tree, there exists at least one node where either `xa = element` or `xb = element`.
- **Non-Membership**: If an element is not in the tree, there exists a node where `xa < element < xb`.



## Append and Delete Proofs

Proofs are available for appending new elements to the tree and deleting existing elements:

- **Append Proof**: If an element is not in the tree, the proof contains the non-membership (update node's) `Val`, the `Val` of the node will be the parent of the new appendable node of the new element, and the minimal subtree of the two `Val`'s nodes.
- **Delete Proof**: If an element is in the tree, the proof contains the two membership (update nodes') `Val`'s, the `Val` of the parent node of the last (in level-oprder) node in the tree, and the minimal subtree of the three `Val`'s nodes.


### Val Types of the Proof

There are two types of Vals in the Proof:

- **Updateable Node's (`Nu`)**: Nodes' val that are already part of the tree but may have their values updated.
- **Parent Node's (`Np`)**: Nodes' val where a new leaf will be appended to or removed from.


## Minimal Subtree (TreeProof)

The proof provided by IntegriTree is a minimal subtree with complexity `3*m*<hash size>*log(n)` (worst-case), where `n` is the size of the tree and `m` is either `2` (append) or `3` (delete) and one (membership-, non-membership proofs) or more (append-, delete proofs) `Val`.

> Note: The minimal subtree is used for proofs and contains only hash values of Vals, nodes and branches along the path of specific nodes (e.g., membership, non-membership, update, delete).

Typescript definition example of the minimal subtree (TreeProof)

``` typescript
export type TreeProof =
  | {
      // `hash` is the valHash of the Val of the node
      HashNode: { hash: string; left: TreeProof; right: TreeProof }
    }
  | {
      // `hash` is the rootHash of the node
      NodeHash: { hash: string }
    }
```

## IntegriTree vs. Minimal Subtree

- **IntegriTree Data Structure**: Represents the complete structure of the tree, including Val, left and right child nodes.
- **Minimal Subtree**: Used for proofs and contains only hash values of vals, nodes and branches along the path of specific nodes.

## Root Hash Calculation

The root hash of the tree or any of its branch is calculated using the following formula:
```
const rootHash = hash(hashVal(nodeVal) || hashRoot(leftChild) || hashRoot(rightChild))
```

## Completeness Enforcement

The tree must always maintain its completeness, enforced by the sequential index of nodes in the tree.


## Technical briefs of the IntegriTree

- The tree model **`T`** representing the set **`S = {x1, ..., xn}`** is and must always be a **`Complete Binary Tree`**.
- A `Complete Binary Tree` can have an incomplete last level, as long as all the leaves in that level are arranged from left to right.
- A **node** (**`N`**) of the `T` tree has two pointers: one to its left child, and one to its right child.
- A **leaf** (**`L`**) is a node  that has two empty hashes (**`ε, ε`**) as its children.
- Every node in `T` must be assigned a sequential index **`i`** starting from **`1`**, with the root of T having index 1, and this index `i` increases by 1 whenever a new node **`Ni`** is added. The value of `i` is always equal to the number of elements in `T`.
- Therefore, the nr. of elements of the tree is always the `tree size - 1`
- The depth (**`d`**) is defined as the length of the simple path (number of edges) from the `root` node of `T` to node `N`.
- The node's index (`i`) must always be exactly double of its parent's index if it's the left child, or 2 times of the parent's index plus 1 if it's the right child.
- The integrity of T is ensured by the sequential index (`i`).
- Therefore the node index determines the position of any node within the 'T' tree and positions in its parent, except for the root node (as it doesn't have a parent).
  - The position of a node in the tree is determined by **`P = i - 2^d + 1`** which means it's the **`Pth`** node on level `d` of the `T` tree.
  - The **left**/**right** position within a node follows the **`even-odd`** rule:  if its index `i` is even, the child is on the left side; otherwise, it's on the right. This position is determined by **`Pn = In - 2Ip`** (`Ip` is the parent's index)  where `Pn` is 0 when the child is on the left, 1 when it's on the right, and results in an **error** otherwise.
- In order to keep track of changes in the state of the `T` tree, there are two types of nodes:
  - Updateable node (**`Nu`**): These nodes are already part of the tree, but their values will be updated sometime by an update process.
  - Appendable node (**`Na`**): These are then nodes where a new leaf `L` will be appended to.
- The right child (`Nr`) of an appendable node (`Na`), must always be an empty hash, represented as **`ε`**. The left child can be either an empty hash (**`ε`**) or a leaf.



## Appendix

### Letter Frequency Table for English Words
 

| Letter | Approximate Frequency |
|--------|------------------------|
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

