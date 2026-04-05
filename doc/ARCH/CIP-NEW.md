---
CIP: XXXX
Title: Efficient Proofs for Dynamic Sets
Status: Proposed
Category: Tools
Authors:
    - Pal Dorogi <pal.dorogi@gmail.com>
Implementors: []
Discussions:
    - https://github.com/cardano-foundation/CIPs/pull/816
Created: 2024-05-17
License: CC-BY-4.0
---

## Abstract

This Cardano Improvement Proposal (CIP) describes a solution for ensuring the uniqueness (at minting time) of dynamically minted NFTs on the Cardano blockchain. 


## Motivation: why is this CIP necessary?

NFTs are unique digital assets that can represent ownership of a wide range of items, such as art, collectibles, and in-game items. One challenge with minting NFTs on the Cardano blockchain is ensuring the uniqueness of each token at minting time. When a user wants to mint an NFT using a decentralized application (dApp), the on-chain code is unable to verify that the NFT being minted with the same policy ID is unique. 
This can lead to the creation of duplicate NFTs, which can cause confusion and disputes over ownership.

This CIP aims to address this challenge by providing a solution for efficiently and securely tracking the dynamic minting and burning of NFTs on the Cardano blockchain. This will ensure that each NFT is truly unique and that ownership is properly recorded.

For complex proposals, a Cardano Problem Statement (CPS) has been prepared to outline the specific challenges and design issues motivating this rework. [Link to CPS]

## Specification

The technical specification details the proposed improvements, including:

On-chain Data Structure: The design defines a compact representation of NFT states, ensuring efficient storage and retrieval.
Off-chain Data Structure: The use of a binary tree facilitates rapid updates and validations of NFT states.
State Machine: A dedicated state machine manages the validation processes, ensuring consistency and uniqueness of NFTs.
Authorization NFT: An NFT that carries the current state, enabling secure and verifiable transactions.
This section also includes a CDDL schema for the on-chain data structures to ensure interoperability among implementations.

## Versioning Requirement

The proposal will address versioning to ensure backward compatibility with existing solutions, as detailed in the optional Versioning section.

## Rationale

This proposal achieves its goals by integrating both off-chain and on-chain mechanisms to efficiently manage the uniqueness of NFTs in a decentralized manner. Before this proposal, there were no existing solutions for decentralized dynamic NFT minting and burning. By utilizing both off-chain and on-chain processes, this approach addresses the scalability and performance limitations inherent in purely on-chain solutions. Although there may be a demand for such functionality, this proposal introduces a novel solution that aims to fulfill the ecosystem's needs for dynamic NFT management.

## Backward Compatibility
The proposal aims to maintain backward compatibility with existing NFT standards, ensuring a smooth transition for current users.

## Path to Active
### Acceptance Criteria
The proposal will become active upon meeting the following criteria:

- Successful implementation and testing of the proposed solution in a live environment.
- Positive feedback from community stakeholders regarding functionality and performance.

### Implementation Plan
The implementation plan will detail the development phases, testing protocols, and community engagement strategies to ensure a successful rollout.



---
CIP: XXXX
Title: Efficient Proofs for Dynamic Sets
Status: Proposed
Category: Tools
Authors:
    - Pal Dorogi <pal.dorogi@gmail.com>
Implementors: []
Discussions:
    - https://github.com/cardano-foundation/CIPs/pull/816
Created: 2024-05-17
License: CC-BY-4.0
---

## Abstract

This Cardano Improvement Proposal (CIP) describes a solution for ensuring the uniqueness of dynamically minted NFTs on the Cardano blockchain. The proposal introduces a method for efficiently managing and validating the dynamic minting and burning of NFTs using a combination of off-chain datasets and on-chain compact representations.

## Motivation: why is this CIP necessary?

Currently, the Cardano blockchain does not allow users to dynamically mint NFTs with the same policy ID. Dynamic minting refers to the ability of users to freely choose and mint NFTs that do not already exist on the blockchain. This is particularly challenging because the on-chain code cannot verify the uniqueness of an NFT at minting time, leading to potential duplicates and ownership disputes.

This CIP aims to address this challenge by providing a solution that leverages off-chain datasets and on-chain compact representations to ensure the uniqueness of each NFT. This approach will enable dynamic minting and burning of NFTs while maintaining the integrity and efficiency of the blockchain.

## Specification

The technical specification details the proposed improvements, including:

- **Off-chain Dataset**: A large dataset containing unique elements that represent the NFTs. This dataset is maintained off-chain to ensure scalability and performance.
- **On-chain Compact Representation**: A compact representation (e.g., accumulator, hash, BLS signature, Zero-Knowledge proof) of the off-chain dataset stored on the blockchain as a state.
- **Plutus Smart Contract**: A smart contract that manages the minting and burning of NFTs. The contract validates the new state from the old state stored on the chain and ensures that the transition is legitimate.
- **Proof of State Transition**: A proof that the new state is a valid transition from the old state, with only the addition or removal of the minting/burning element. This proof ensures that the new state accurately reflects the updated off-chain dataset.

## Versioning Requirement

The proposal will address versioning to ensure backward compatibility with existing solutions, as detailed in the optional Versioning section.

## Rationale

This proposal achieves its goals by integrating off-chain datasets and on-chain compact representations to efficiently manage the uniqueness of NFTs in a decentralized manner. By utilizing both off-chain and on-chain processes, this approach addresses the scalability and performance limitations inherent in purely on-chain solutions. The use of compact representations and proofs ensures that the validation process is efficient and secure.

## Backward Compatibility

The proposal aims to maintain backward compatibility with existing NFT standards, ensuring a smooth transition for current users.

## Path to Active

### Acceptance Criteria

The proposal will become active upon meeting the following criteria:

- Successful implementation and testing of the proposed solution in a live environment.
- Positive feedback from community stakeholders regarding functionality and performance.

### Implementation Plan

The implementation plan will detail the development phases, testing protocols, and community engagement strategies to ensure a successful rollout.
