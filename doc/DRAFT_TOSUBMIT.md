---
CPS: ????
Title: Efficient Decentralized Solutions for Dynamic NFT Minting on Cardano
Status: Open
Category: Tokens
Authors:
  - Pal Dorogi<pal.dorogi@gmail.com>
Proposed Solutions:
  - CIP-????: https://github.com/cardano-foundation/CIPs/pull/918
Discussions:
  - https://github.com/cardano-foundation/CIPs/pull/861
Created: 2024-09-26
License: CC-BY-4.0
---


# Cardano Policy Statement (CPS)

## Problem

### Background

This Cardano Policy Statement (CPS) addresses the challenge of ensuring the uniqueness of dynamically minted NFTs on the Cardano blockchain. The current Extended UTxO (EUTxO) model restricts dynamic minting due to its deterministic nature, making it impossible to verify the uniqueness of NFTs on-chain, at minting time, without access to the entire ledger state.

### Reference to CIP

This CPS is an extension of the Cardano Improvement Proposal (CIP) titled "Efficient Proofs for Dynamic Minting and Burning of NFTs" (CIP-XXXX). The CIP proposes a solution that leverages an off-chain dataset and on-chain compact representation of the dataset together with a cryptographic proofs to enable decentralized and scalable dynamic minting and burning of NFTs.

### Problem Statement

The primary issue is the inability to dynamically mint NFTs with the same policy ID while ensuring their uniqueness. This limitation constrains the development of decentralized applications that require unique NFTs, such as decentralized usernames or collectible assets. The CPS aims to provide a detailed implementation plan and rationale for the solution proposed in the CIP.

## Use Cases

### Potential Solutions

The proposed solution involves an off-chain dataset of all previously minted and burned NFTs, which can be rebuilt by anyone from past transactions. A compact representation of this dataset is stored on-chain as the state of the dataset, and efficient cryptographic proofs are used to validate state transitions during minting and burning.

This approach can be applied to any protocol that supports a compact representation of large datasets, along with efficient proofs for additions and deletions.

The current implementation leverages `IntegriTree` (detailed in later sections) and is available for review at the [Adatag GitHub Repository](https://github.com/ilap/adatag).

Other potential candidates that have the required properties could be:
- [Sparse Merkle Tree (SMT)](https://github.com/aiken-lang/sparse-merkle-tree)
- [Merkle Patricia Forestry (MPF)](https://github.com/aiken-lang/merkle-patricia-forestry)
- [BLS-based Plutus Accumulator](https://github.com/perturbing/plutus-accumulator)


### Alternative Methods

Current alternative methods, such as [AdaHandle](https://handle.me), rely on centralized control mechanisms to ensure the uniqueness of NFTs. These approaches are not fully decentralized and introduce risks, including single points of failure, which can compromise security and resilience. Such methods are unsuitable and highly discouraged within a decentralized blockchain ecosystem like Cardano, where decentralization and trustless mechanisms are fundamental principles.

## Goals

### Success Criteria

The success criteria for any proposed solution include:

1. **Decentralization**: The solution must be fully decentralized, without relying on centralized control mechanisms.
2. **Scalability**: The solution must be scalable, capable of handling a growing number of NFTs over time.
3. **Efficiency**: The solution must be efficient in terms of on-chain storage and computational requirements.
4. **Transparency**: The off-chain dataset must be transparent and verifiable by anyone.
5. **Security**: The solution must ensure the uniqueness of NFTs and prevent duplicate minting.

### Progress and Projects

Several projects implement NFT minting solutions, but they often rely on centralized mechanisms or do not address dynamic minting capabilities.

While these projects may provide partial solutions, they fall short of the goals outlined in this CPS. This proposal aims to deliver a comprehensive solution that meets all the defined success criteria, ensuring full decentralization, scalability, and efficiency for dynamic NFT minting.

Adatag is the first project, as far as I know, for tackling this issue.


### Ranking of Goals

1. **Decentralization**: Ensuring the solution is fully decentralized is the most critical goal.
2. **Security**: Preventing duplicate minting and ensuring NFT uniqueness is essential.
3. **Scalability**: The solution should be able to handle a large number of NFTs efficiently.
4. **Efficiency**: Minimizing on-chain storage and computational requirements is crucial.
5. **Transparency**: Ensuring the off-chain dataset is transparent and verifiable.

### Metrics

The success of the goals can be measured using the following metrics:

- **Decentralization**: The number of independent nodes maintaining the off-chain dataset.
- **Security**: The absence of duplicate NFTs and successful validation of state transitions.
- **Scalability**: The ability to handle a large number of NFTs without significant performance degradation.
- **Efficiency**: The size of cryptographic proofs and the computational cost of on-chain validation.
- **Transparency**: The ease of reconstructing and verifying the off-chain dataset.

## Open Questions

### Identifying Possible Solutions

1. What are the most efficient cryptographic proofs for validating state transitions in the context of dynamic NFT minting and burning?
2. How can the off-chain dataset be maintained and synchronized across multiple nodes to ensure data integrity?
3. What are the potential vulnerabilities in the proposed solution, and how can they be mitigated?
4. How can the solution be optimized to minimize on-chain storage and computational requirements?
5. What are the best practices for bootstrapping the initial state of the off-chain dataset?

### Knowledge and Experience

The list of questions reflects the knowledge and experience of possible vulnerabilities and design flaws in the proposed solution. Addressing these questions will help refine the implementation and ensure the robustness of the system.

## Specification

### Data Structures

The solution uses the following data structures:

- **Complete Binary Hash Tree**: The tree is used to accumulate the elements in the set and to provide proofs.
- **Open-Interval Pairs**: A pair of elements, in which the first element is less than the second element, and the two elements are consecutive. The pairs are used to represent the values in the complete binary hash tree and to enforce the uniqueness of the elements in the set.
- **Accumulator**: A single hash value that represents the state of the complete binary hash tree. The accumulator is stored on the blockchain and is updated whenever an element is added or removed from the set.
- **Proof**: A set of hashes that allows the verification of a particular node in the complete binary hash tree, without requiring the entire tree. The proof is generated when an element is added or removed from the set and is sent to the blockchain along with the updated accumulator.

### Key Features

- Unlike traditional hash Merkle trees, the solution stores values in every node.
- These values are represented as (open-interval) pairs of consecutive elements with each node in the tree.
- Offers proofs for membership, non-membership, addition, and deletion.
- Proof size scales logarithmically with the number of elements.

### Brief Overview of Proofs

The accumulator `accT` of the tree `T` is the `hashRoot(T)`, calculated by combining the hashes of the node's value, the hashRoot of its left child, and the hashRoot of its right child: `accN = hash( hashVal(value) || hashRoot(leftChild) || hashRoot(rightChild))`, where `||` represents the concatenation of byte arrays.

The proof provided by the solution consists of a compact path (minimal subtree) from the root node `Nr` to the provable node(s), ensuring that the root hash of this subtree matches the root hash of the entire tree `T`, and at least one (depends on the proof type) node's value of the provable node is included in the proof.

### Implementation

This scheme is implemented within the Cardano blockchain system. The blockchain will store only the states of the tree `T`, which include:

- The accumulator (root hash) of the tree `T`.
- The operation performed (added/deleted).
- The current size of the tree.
- The element added or deleted.

And the proof of the state transition, which include:
- The minimal subtree required for validation, together with the 
- relevant `Val` parameters.

The state change will be bound to and carried by an authorization token held at the relevant script address that is involved in the transition validation.

Each state change (element added or removed) is validated by the corresponding Plutus minting (mint/burn) and state holder (script) . This validation process is based on the old state, the user's input as redeemer, and the provided new state by the user(s) as an inline datum of the Extended UTXO (EUTxO) containing the authorization token.

### Complete Binary Tree vs. Minimal Subtree

- **Complete Binary Tree Data Structure**: Represents the complete structure of the tree, including Val, left and right child nodes.
- **Minimal Subtree**: Used for proofs and contains only hash values of vals, nodes, and branches along the path of specific nodes.

### Node Structure

Each node in the Complete binary tree (name IntegriTree) consists of:
- `Val`: A data structure containing information about the node, including the index of the node in the tree in level-order i.e., size of the tree (`xi`) and the pairs of consecutive elements (as two bounds) of the open-interval (`xa` and `xb`).
- `Children`: left and right child nodes.

Typescript's example:
```typescript
export type Val = { xi: string; xa: string; xb: string }

export type IntegriTree = {
  val: Val;
  left: IntegriTree;
  right: IntegriTree;
}
```

### Root Hash Calculation of IntegriTree
The off-chain root hash calculation of the tree or any of its branches is calculated using the following function:

``` js
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


### Minimal Subtree (Proof)

The proof provided by the solution is a minimal subtree together with the required values (depends on the proofs). In other words, the minimal subtree is a path from the root node to the provable node (membership or non-membership) contains hash of the nodes' value the hash of the child not in the path down to the node and the child contains the node as path down to the provable node.

Typescript definition example of the minimal subtree (TreeProof):

``` typescript
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

## Root Hash Calculation of the Minimal Subtree

The root hash of the minimal subtree uses a similar function as in the IntegriTree:

``` 
pub fn root_hash(root: Proof) -> Hash {
  when root is {
    NodeHash { hash } -> hash
    HashNode { hash, left, right } ->
      combine_three_hashes(hash, root_hash(left), root_hash(right))
  }
}
```

### Membership and Non-Membership Proofs

IntegriTree provides proofs for both membership and non-membership of elements in the tree:

- **Membership**: If an **`x`** element is in the tree, there exists at least one node where either **`xa = x`** or **`xb = x`**.
- **Non-Membership**: If an element is not in the tree, there exists a node where **`xa < x < xb`**.

## Append and Delete Proofs

Proofs are available for appending new elements to the tree and deleting existing elements:

- **Append Proof**: If an element is not in the tree, the proof contains the non-membership (update node's) value **`Vu`**, the value **`Vp`** of the node will be the parent of the new appendable node of the new element, and the minimal subtree of the two **`Val`**'s nodes.
- **Delete Proof**: If an element is in the tree, the proof contains the two membership (update nodes') values the **`Vu1`** and **`Vu2`** (the **`(xα, x)`** and **`(x, xβ)`**), and the value **`Vp`** the parent node of the last (in level-order) node in the tree, and the minimal subtree create from the nodes of the three values.

### Val Types of the Proof
There are two types of Vals used with the Proof:

- **Updateable Node's (`Nu`'s)**: Nodes' val that are already part of the tree but may have their values updated.
- **Parent Node's (`Np`'s)**: Nodes' val where a new leaf will be appended to or removed from.

#### Completeness Enforcement

The tree must always maintain its completeness, which is enforced by the sequential index of nodes in the tree.

### Technical Briefs of the IntegriTree

- The tree model `T` representing the set `X = {x1, ..., xn}` is and must always be a **Complete Binary Tree**.
- A **Complete Binary Tree** can have an incomplete last level, as long as all the leaves in that level are arranged from left to right.
- A **node** (`N`) of the `T` tree has two pointers: one to its left child, and one to its right child.
- A **leaf** (`L`) is a node that has two empty hashes (`ε, ε`) as its children.
- Every node in `T` must be assigned a sequential index `i` starting from `1`, with the root of `T` having index `1`, and this index `i` increases by `1` whenever a new node `Ni` is added. The value of `i` is always equal to the number of nodes (and not the number of elements) in `T`.
- Therefore, the number of elements of the tree is always the `tree size - 1`.
- The **depth** (`d`) is defined as the length of the simple path (number of edges) from the `root` node of `T` to node `N`.
- The node's index (`i`) must always be exactly double of its parent's index if it's the left child, or `2` times of the parent's index plus `1` if it's the right child.
- The integrity of `T` is ensured by the sequential index (`i`).
- Therefore, the node index determines the position of any node within the `T` tree and positions in its parent, except for the root node (as it doesn't have a parent).
  - The position of a node in the tree is determined by `P = i - 2^d + 1` which means it's the `Pth` node on level `d` of the `T` tree.
  - The **left**/**right** position within a node follows the **even-odd** rule: if its index `i` is even, the child is on the left side; otherwise, it's on the right. This position is determined by `Pn = In - 2Ip` (`Ip` is the parent's index) where `Pn` is `0` when the child is on the left, `1` when it's on the right, and results in an **error** otherwise.
- In order to keep track of changes in the state of the `T` tree, there are two types of nodes:
  - **Updateable node** (`Nu`): These nodes are already part of the tree, but their values will be updated sometime by an update process.
  - **Parent node** (`Np`): These are then nodes where a new leaf `L` will be appended to or the last leaf removed from.
- The right child (`Nr`) of a parent node (`Np`), must always be an empty hash, represented as `ε`. The left child can be either an empty hash (`ε`) or a leaf.


## Rationale

The solution described in this CPS achieves the goal of ensuring the uniqueness of NFTs minted on the Cardano blockchain by using an off-chain data structure to track the minting and burning of NFTs, and storing the hash of this data structure together with the proofs on the blockchain. This allows the on-chain code to verify the uniqueness of an NFT at minting time, without requiring the entire data structure to be stored on the blockchain.

Also, it allows anyone to independently build the off-chain data tree by retrieving on-chain minting/burning transactions. Therefore, there is no need for centralized solutions to validate the integrity of the tree, promoting decentralization and transparency within the Cardano ecosystem. Though, any - even un-trusted or adversarial - 3rd-party services can be used to be queried for a proof.

The use of IntegriTree as the off-chain data structure allows for efficient and secure tracking of the minting and burning of NFTs. IntegriTree is a new accumulator scheme that leverages a complete binary hash tree for compact accumulation and dynamic updates.

The tree is used to accumulate the elements in the set, and to provide proofs. The values are represented as open-interval pairs of consecutive elements with each node in the tree. IntegriTree offers proofs for membership, non-membership, addition, and deletion, with a proof size that scales logarithmically with the number of elements.

The use of an accumulator to represent the state of the IntegriTree allows for the updated state to be verified and recorded on the blockchain in a single transaction. This simplifies the process of updating the state, and reduces the risk of errors or disputes.

## Path to Active

### Acceptance Criteria

The acceptance criteria for this CPS to become active are as follows:
- [ ] The solution described in the CPS has been implemented and tested on the Cardano blockchain.
- [ ] The implementation has been reviewed and approved by subject matter experts.
- [ ] The community has had sufficient time to review and provide feedback on the CPS and the implementation.
- [ ] Any concerns or issues raised during the review and testing process have been addressed and resolved.

### Implementation Plan

The implementation plan for this CPS is as follows:
- [ ] The solution described in the CPS will be implemented and tested - on the Cardano blockchain by the proposer of the CPS.
- [ ] The proposer will submit the implementation for review and approval by subject matter experts.
- [ ] The proposer will make the implementation and the CPS available for review and feedback by the community.
- [ ] The proposer will address and resolve any concerns or issues raised during the review and testing process.
- [ ] Once the acceptance criteria have been met, the proposer will submit a pull request to the Cardano Improvement Proposals repository to update the status of the CPS to active.

## References
- [Adatag MVP/PoC on Preview Network](https://adatag.io)
- [Adatag Github Repository](https://github.com/ilap/adatag)
- [Minimal Subtrees](https://link.springer.com/chapter/10.1007/978-3-319-61199-0_8)
- [Strong Accumulators from Collision-Resistant Hashing](https://users.dcc.uchile.cl/~pcamacho/papers/strongacc08.pdf)
- [CONIKS: Bringing Key Transparency to End Users](https://www.usenix.org/system/files/conference/usenixsecurity15/sec15-paper-melara.pdf)
- [Cryptography for Efficiency: New Directions in Authenticated Data Structures](https://user.eng.umd.edu/~cpap/published/theses/cpap-phd.pdf)
- [Transparency Logs via Append-Only Authenticated Dictionaries](https://eprint.iacr.org/2018/721.pdf)
- [Batching non-membership proofs and proving non-repetition with bilinear accumulators](https://eprint.iacr.org/2019/1147.pdf)

## Credits

- [Sparse Merkle Tree (SMT)](https://github.com/aiken-lang/sparse-merkle-tree)
- [Merkle Patricia Forestry (MPF)](https://github.com/aiken-lang/merkle-patricia-forestry)
- [BLS-based Plutus Accumulator](https://github.com/perturbing/plutus-accumulator)



## Versioning

The solution described in this CIP does not require any specific versioning approach.

## Copyright

This CIP is licensed under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/legalcode)