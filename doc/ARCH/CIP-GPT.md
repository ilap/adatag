---
CIP: ?
Title: Decentralized Dynamic Minting and Burning of NFTs on Cardano
Status: Proposed
Category: Ledger
Authors: [Your Name, your.email@example.com]
Implementors: []
Discussions: [link to discussions or PRs]
Solution-To: []
Created: 2024-09-24
License: CC-BY-SA-4.0
---

# Abstract

This CIP proposes a decentralized solution for dynamically minting and burning non-fungible tokens (NFTs) on the Cardano blockchain using the EUTxO model. The solution involves maintaining an off-chain dataset, whose integrity is represented on-chain by a compact accumulator, stored within an unspent transaction output (UTxO). This UTxO is anchored to a preminted authorization NFT (auth token). The Plutus script ensures secure and verifiable minting or burning of NFTs based on valid proofs of changes to the dataset, enabling fully decentralized and trustless operations without reliance on any centralized entity.

# Motivation

Current NFT minting and burning mechanisms in Cardano are not dynamic or decentralized, requiring users to rely on third-party entities or platforms for these operations. This CIP aims to introduce a self-sovereign solution for NFT management, where users can dynamically mint or burn NFTs (such as unique usernames) without involving any trusted authority. By using a Plutus smart contract, the solution provides verifiable on-chain integrity of the off-chain dataset, enabling operations such as minting when a username is available or burning when it is not.

This approach opens up new use cases for decentralized, dynamic asset management, improving the flexibility and decentralization of NFT minting for Cardano applications.

# Specification
Data Structure and State Representation
Off-chain Dataset: The dataset is maintained off-chain and consists of elements such as available usernames. Each username follows a strict pattern (max 16 characters, lowercase letters, numbers, and dashes).
Compact Representation (Accumulator): A cryptographic hash or accumulator represents the state of the off-chain dataset. This accumulator is stored on-chain and updated when changes (addition/removal of an element) occur.
Authorization NFT (Auth Token): The state is stored in a UTxO linked to an auth token, an NFT representing the integrity of the dataset. This token ensures that only authorized operations can modify the state.


Transaction Flow
Minting:

The user submits a transaction to mint a new NFT (e.g., a username).
The transaction must include:
The current state of the dataset (accumulator).
Proof that the new element (username) is not already in the dataset.
The new accumulator representing the updated dataset (with the new element).
The authorization NFT (auth token) as input.
The Plutus script verifies the proof, ensuring that the new state is a valid extension of the old state with the addition of the element.
If valid, the transaction mints the new NFT and updates the accumulator on-chain.
Burning:

Similar to minting, except the proof shows that the element (username) has been removed from the dataset.
The new accumulator reflects this removal, and the NFT associated with the element is burned.
State Machine Implementation
The entire process operates as a state machine:

Old State: Represents the dataset before any operation (stored in a UTxO).
New State: Represents the updated dataset after the operation.
Transition Proof: Validates the transition between the old and new states (addition or removal of an element).
The on-chain Plutus script ensures the integrity of the proof, checking that old_state == prove(new_state, addition/deletion proof).

CDDL Schema
The data structures (e.g., the accumulator, NFT metadata) will be defined in CDDL for clarity and interoperability across different implementations.

Versioning
The implementation must define versioning within the state representation (accumulator), allowing future protocol upgrades to extend functionality without breaking backward compatibility.

# Rationale
This CIP leverages the EUTxO model's strengths for secure and verifiable transactions without a central authority. The use of an auth token prevents unauthorized changes to the state, while the state machine ensures that the dataset's integrity remains intact during operations.

Alternative designs considered include centralized solutions or reliance on third-party entities, which are less secure and decentralize control. This solution provides a self-sovereign approach, aligning with Cardano's decentralized philosophy.

The solution also extends the CPS for dynamic NFT management by introducing formalized minting and burning mechanisms, ensuring flexibility and trustless operation.

# Path to Active
## Acceptance Criteria

- Successful deployment and testing of the Plutus smart contract to mint/burn NFTs on a public testnet.
- Community review and consensus.
- Demonstrated interoperability with existing Cardano wallets and tools.

## Implementation Plan
- Development of a Plutus script that implements the state machine described in this CIP.
- Implementation of a client-side tool to handle off-chain dataset management and proof generation.
- Testing in public testnet environments for validation.

## Acknowledgements
Thanks to the Cardano community for providing feedback and support during the drafting of this proposal.

# Appendices

Details of cryptographic proofs and accumulators used for state verification.
## Copyright
This CIP is licensed under CC-BY-SA-4.0.

