<div align="center">
  <h1 align="center">@Adatag</h1>
    <h2 align="center" style="border-bottom: none">A unique username for everyone.</h2>

[![License](https://img.shields.io/github/license/ilap/adatag)](https://github.com/ilap/adatag/blob/main/LICENSE)

</div>

## Introduction
Welcome to **@Adatag**, a web3 username service built on the Cardano blockchain that enables users to manage unique, NFT-based usernames.
It utilizes [IntegriTree](#integritree) to ensure the uniqueness and integrity of usernames.

## Adatag Features

- **Decentralized**: @Adatag is a ***real*** dApp, not controlled by any single entity or keys.
- **Transparent**: @Adatag is completely open-source to allow anybody to validate and audit its logic.
- **Free**: @Adatag username creation (minting) and deletion (burning TBD) are completely free, with no price or royalty fees.
- **Instant**: @Adatag usernames - if available - can be created and deleted instantly, without any third-party involvement.


> Note: Users must time-lock deposit a certain value (depending on the length of the @Adatag) to prevent abuse of the system. These deposits are redeemable by the rightful beneficiaries after a certain time (preferably 20 days) of minting.

## IntegriTree

IntegriTree is a complete-binary tree-based data structure that stores only unique elements using open-interval, featuring a short accumulator (hash of the tree) and proofs (member, non-member, addition, and deletion proofs) that can be stored on-chain, similar to Merkle Tree, Trie, or Patricia Trees proofs.

## How @Adatag Works

Using @Adatag involves bootstrapping, which includes generating the required Plutus scripts based on the protocol parameters and deploying them onto the Cardano blockchain along with the initial state(s) of the protocol.

- Protocol State:  Initially, the protocol state is based on the relevant trees containing only one initial node that includes the lower and upper bounds of the open interval (e.g., like a `[-Infinity, +Infinity]` open interval). This initial bootstrap is crucial for ensuring the integrity of the system.
- User-Generated Proofs: Users generate proofs off-chain, which the blockchain smart contract validates. This eliminates the need for trusted users or third parties, as the chain only stores and handles states with valid proofs. These can be easily reconstructed and validated by building up the tree from the minted and burned elements.
- Enforced Integrity Logic: The integrity of the tree is maintained through specific logic implemented within the smart contracts.


For more details, refer to the [Design Specification](./doc/DESIGNSPEC.md) and [IntegriTree Technical Specification](./doc/TECHSPEC.md).

### Monorepo Contents

This repository contains everything needed for @Adatag, serving as a Minimum Viable Product (MVP). It includes:

- [**Bootstrap (@adatag/deploy)**](./apps/deploy/): The source code for bootstrapping the protocol.
- [**IntegriTree TypeScript Implementation (@adatag/integri-tree)**](./libs/integri-tree/): Source code demonstrating the implementation of IntegriTree.
- [**Smart Contract Source Code (aiken)**](./contracts/aiken): All smart contracts written in Aiken (Plutus deprecated) for validating username creation and deletion (TBD).
- [**Demo Website (@adatag/adatag.io)**](./apps/adatag.io): The source code for the demo website will be hosted at [adatag.io](https://adatag.io), providing users with a user-friendly interface for interacting with the dApp.

## Getting Started

To get started with @Adatag, ensure you have the necessary dependencies installed:
- [git](https://git-scm.com/download/): A free and open-source distributed version control system.
- [docker](https://docs.docker.com/engine/install/): Docker engine for developing or demoing (using Yaci-devkit based private Cardano blockchain)
- [bun](https://bun.sh): A fast all-in-one JavaScript runtime.
- [Node.js (v18)](https://nodejs.org/en/download/package-manager/): JavaScript runtime.
- [Aiken](https://aiken-lang.org/installation-instructions): A modern smart contract platform for Cardano.

For more details, read the [Development Environment Overview](./tools/README.md)

> Note: These dependencies are essential for setting up and running @Adatag effectively.

### Local Demo

For demoing @Adatag, please follow these steps (assuming all prerequisites are completed):

```bash
$ git clone https://github.com/ilap/adatag
$ cd adatag
$ bun i && npm i
$ npx nx run @Adatag/adatag.io:serve:custom
...
> nx run @Adatag/adatag.io:serve

> vite serve

  VITE v5.2.10  ready in 397 ms

  ➜  Local:   http://localhost:4200/
  ➜  Press h + enter to show help
```

## Contribution

Contributions to the @adatag IntegriTree PoC and MVP implementation are welcome! Whether it's bug fixes, feature enhancements, or documentation improvements, feel free to submit an [issue](https://github.com/ilap/adatag/issues) or [pull request](https://github.com/ilap/adatag/pulls) to help improve the project.

## License

This project is licensed under the [MIT License](LICENSE).
