<div align="center">
  <h1 align="center">@adatag</h1>
    <h2 align="center" style="border-bottom: none">A unique username for everyone.</h2>

[![Licence](https://img.shields.io/github/license/ilap/aiken)](https://github.com/ilap/adatag/blob/main/LICENSE)

</div>

## Introduction
Welcome to @adatag, the IntegriTree Proof of Concept (PoC)! This project serves as an example use case of the IntegriTree, a tree-based data structure designed to contain only unique elements. @adatag MVP implementation utilizes IntegriTree to manage unique usernames as non-fungible tokens (NFTs) on the Cardano blockchain.


## Features

- **Decentralized**: @adatag is not controlled by any single entity, making it more secure and resistant to censorship.
- **Free**: @adatag username creation and deletion is completely free, with no price or royalty fees.
- **Instant**: @adatag usernames can be created and deleted instantly, without any third-party involvement.
- **Public**: @adatag usernames are publicly stored on-chain, making them accessible to everyone.

> Note: Initially, users must time-lock deposit a certain value to prevent abuse of the system. These deposits are redeemable by the rightful beneficiaries after a certain time (preferably 20 days) of minting.

## IntegriTree Data Structure
@adatag employs a complete binary data structure called IntegriTree for ensuring the integrity of the system. Unlike traditional Merkle trees where the blockchain generates the tree and users validate its integrity, IntegriTree works differently.

### How IntegriTree Works
  - Bootstrap Protocol: Initially, the proof stored on the chain is a tree containing only one initial node. This initial bootstrap is crucial for ensuring the integrity of the system.
  - User-Generated Proofs: Users generate proofs off-chain, which the blockchain smart contract validates. This eliminates the need for trusted users or third parties, as the chain only stores and handles states that have valid proofs.
   - Enforced Integrity Logic: The integrity of the tree is enforced by specific logic implemented within the smart contracts.

  See details in the [IntegriTree Technical Specification](./doc/TECHSPEC.md)

### Repository Contents

This repository contains everything needed for the Cardano Username Service dApp, serving as a Minimum Viable Product (MVP). It includes:

- [**IntegriTree TypeScript Implementation**](./libs/integri-tree/): Source code demonstrating the implementation of IntegriTree.
- [**Smart Contract Source Code**](./contracts/aiken): All smart contracts written in Aiken (Plutus deprecated) for validating username creation and deletion.
- [**adatag.io (Demo Website)**](./apps/adatag.io): The source code for the demo website will be hosted at [adatag.io](https://adatag.io), providing users with a user-friendly interface for interacting with the dApp.

## Getting Started

To get started with @adatag, follow these steps:

1. Clone the repository to your local machine.
2. Install the necessary dependencies:
   - [bun](https://github.com/bundler/bundler): Dependency manager for Ruby projects.
   - [aiken](https://example.com): Aiken is a tool for ...
   - [node.js](https://nodejs.org/): JavaScript runtime for executing JavaScript code outside of a web browser.
3. Explore the IntegriTree implementation and its usage within the demo website.
4. Experiment with the functionalities provided by IntegriTree for managing unique elements.

## Contribution

Contributions to the @adatag IntegriTree PoC and MVP implementation are welcome! Whether it's bug fixes, feature enhancements, or documentation improvements, feel free to submit pull requests to help improve the project.

## License

This project is licensed under the [MIT License](LICENSE).
