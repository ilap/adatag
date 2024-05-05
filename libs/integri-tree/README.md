# Introducing IntegriTree: A Logarithmically Efficient Proof Scheme for Dynamic Sets

We propose IntegriTree, a new accumulator scheme for verifying sets of elements. It utilizes a complete binary-based hash tree as a compact accumulator and relies solely on a secure hash function.

Unlike traditional hash trees, such as the Merkle tree, where values are stored only in the leaves, IntegriTree stores values in every node of the tree. This enables efficient dynamic updates, allowing elements to be added or removed as needed.

Additionally, instead of storing individual values directly, IntegriTree associates (open-interval) pairs of consecutive elements with each node in the tree. This approach facilitates efficient proofs of membership, non-membership, addition, and deletion.

For example, to demonstrate the absence or presence of an element `x` in the accumulated set, one must prove that a pair `(xα, xβ)`, where `xα ≺ x ≺ xβ` (absence), or either of the pairs `(xα, x)` and `(x, xβ)`, are present in the tree.

Importantly, these proofs are significantly smaller than the entire set, typically scaling at a logarithmic rate with the number of elements.

## Implementation

We presume that the Cardano blockchain provides the necessary security levels to ensure that all participants have access to published information and that no one can delete a submitted transaction.

As a result, our implementation relies on the Cardano blockchain and the corresponding Plutus smart contract to ensure that the publication of successive accumulator values corresponding to updates of the set cannot be falsified. Specifically, even if an adversary attempts to send invalid values to the blockchain, they cannot publish altered accumulator values.

Our implementation of the dynamic universal accumulator offers the following functionalities:

- Adding and removing elements from the accumulated set is supported.
- Addition or removal of an element from the accumulated set can be proven.
- All elements of the set are accumulated into a single concise value.
- A proof exists for every element, proving whether it has been accumulated or not.
