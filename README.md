<div align="center">
  <h1 align="center">@Adatag</h1>
    <h2 align="center" style="border-bottom: none">A unique username for everyone.</h2>

[![Demo](https://img.shields.io/badge/demo-online-brightgreen.svg?style=for-the-badge)](https://adatag.io)
[![Licence](https://img.shields.io/github/license/aiken-lang/stdlib?style=for-the-badge)](https://github.com/ilap/bls/blob/main/LICENSE)
[![Code size](https://img.shields.io/github/languages/code-size/ilap/adatag.svg?style=for-the-badge)](https://github.com/ilap/adatag)

</div>

# Introduction

Welcome to **@Adatag**, a web3 username service built on the Cardano blockchain that enables users to manage unique, NFT-based usernames.
It utilizes [IntegriTree](#integritree) to ensure the uniqueness and integrity of usernames.

To our knowledge, @Adatag is the first decentralised application (dApp) on the Cardano blockchain to enable dynamic minting of non-fungible tokens (NFTs) using the same policy ID, without requiring any third-party involvement.

Note: This project has been updated to work with the **Conway** era by migrating from the `@translucent` library to the `@blaze-cardano` library.

**Before We Begin, Please Review Our Core Values Below**:

# Our Core Values

- **Transparency**: We are fully committed to openness in all our operations and communications.
- **Accountability**: We take full responsibility for our actions and decisions.
- **Intellectual Honesty**: We are committed to being truthful about our capabilities and limitations.
- **Decentralisation**: We only support decentralised solutions and therefore firmly reject centralised alternatives, including projects that are not open-source. I.e. we reject any form of centralisation that compromises the decentralised nature of Cardano blockchain and our project(s).
- **Integrity**: Our actions consistently align with our core values.
- **Inclusivity**: We promote a culture where everyone is welcome and included without exception.
- **Humility**: We approach our work with modesty and reject all forms of arrogance. Remember, overestimating our intelligence is often a sign of foolishness.
- **Openness**: We prioritize open-source development to encourage collaboration, transparency, and community-driven innovation.

**We strictly enforce these principles and do not tolerate deviations, especially closed-source or non-transparent centralised solutions.**

**Additionally, we believe that genuine intelligence includes humility and collaboration, not arrogance or a sense of superiority.**

# Adatag Features

- **Decentralised**: @Adatag is a **_real_** dApp, not controlled by any entity or keys.
- **Transparent**: @Adatag is completely open-source to allow anybody to validate and audit its logic.
- **Free**: @Adatag username creation (minting) and deletion (burning TBD) are completely free, with no price or royalty fees.
- **Instant**: @Adatag usernames - if available - can be created and deleted instantly, without any third-party involvement.

> Note: Users must time-lock deposit a certain value (depending on the length of the @Adatag) to prevent abuse of the system. These deposits are redeemable by the rightful beneficiaries after a certain time (preferably 20 days) of minting.

# IntegriTree

IntegriTree is a complete-binary tree-based data structure that stores only unique elements using open-interval, featuring a short accumulator (hash of the tree) and proofs (member, non-member, addition, and deletion proofs) that can be stored on-chain, similar to Merkle Tree, Trie, or Patricia Trees proofs.

# How @Adatag Works

Using @Adatag involves bootstrapping, which includes generating the required Plutus scripts based on the protocol parameters and deploying them onto the Cardano blockchain along with the initial state(s) of the protocol.

- Protocol State: Initially, the protocol state is based on the relevant trees containing only one initial node that includes the lower and upper bounds of the open interval (e.g., like a `[-Infinity, +Infinity]` open interval). This initial bootstrap is crucial for ensuring the integrity of the system.
- User-Generated Proofs: Users generate proofs off-chain, which the blockchain smart contract validates. This eliminates the need for trusted users or third parties, as the chain only stores and handles states with valid proofs. These can be easily reconstructed and validated by building up the tree from the minted and burned elements.
- Enforced Integrity Logic: The integrity of the tree is maintained through specific logic implemented within the smart contracts.

For more details, refer to the [Design Specification](./doc/DESIGNSPEC.md) and [IntegriTree Technical Specification](./doc/TECHSPEC.md).

# Monorepo Contents

This repository contains everything needed for @Adatag, serving as a Minimum Viable Product (MVP). It includes:

- [**Bootstrap (@adatag/deploy)**](./apps/deploy/): The source code for bootstrapping the protocol.
- [**IntegriTree TypeScript Implementation (@adatag/integri-tree)**](./libs/integri-tree/): Source code demonstrating the implementation of IntegriTree.
- [**Smart Contract Source Code (aiken)**](./contracts/aiken): All smart contracts written in Aiken (Plutus deprecated) for validating username creation and deletion (TBD).
- [**Demo Website (@adatag/adatag.io)**](./apps/adatag.io): The source code for the demo website will be hosted at [adatag.io](https://adatag.io), providing users with a user-friendly interface for interacting with the dApp.

# Getting Started

To get started with @Adatag, ensure you have the necessary dependencies installed:

- [git](https://git-scm.com/download/): A free and open-source distributed version control system.
- [docker](https://docs.docker.com/engine/install/): Docker engine for developing or demoing (using Yaci-devkit based private Cardano blockchain)
- [bun](https://bun.sh): A fast all-in-one JavaScript runtime.
- [Node.js (v20)](https://nodejs.org/en/download/package-manager/): JavaScript runtime.
- [Aiken](https://aiken-lang.org/installation-instructions): A modern smart contract platform for Cardano.

For more details, read the [Development Environment Overview](./tools/README.md)

> Note: These dependencies are essential for setting up, running and developing @Adatag effectively.

# Local Demo for @Adatag

To demo @Adatag on a custom private Cardano blockchain using Yaci Devkit, follow these steps (assuming all prerequisites are completed):

1. Custom Nami Wallet: Since Yaci Devkit does not fully support Blockfrost API, and cannot use custom API endpoints, a custom-built Nami wallet is required for the demo.

- Clone the repository:
  ```bash
  git clone http://github.com/ilap/nami
  ```
- Navigate to the Nami's directory:

  ```bash
  cd nami
  ```

- Copy the testing secrets file:

  ```bash
  cp secrets.testing.js secrets.production.js
  ```

- Switch to the `feature-yaci` branch

  ```bash
  git checkout feature-yaci
  ```

- Install dependencies and build the wallet (Nami requires Node.js v20):
  ```bash
  npm i && npm run build
  ```
- Add the custom Nami wallet to Chrome/Brave extensions:

  - Open the extensions page: `brave://extensions/` or `chrome://extensions/`
  - Enable "Developer mode" by toggling the switch in the top-right corner
  - Click "Load unpacked" and select the `./build` directory from the Nami directory.

- Create a wallet using the following seed:

  ```text
  test test test test test test test test test test test test test test test test test test test test test test test sauce
  ```

- Switch to the `Custom` network in Nami:
  `Top Right Avatar -> Settings -> Network -> Custom Node`

2. Build and run @Adatag on custom private Cardano blockhain:

- Clone the repository:

  ```bash
  $ git clone https://github.com/ilap/adatag
  ```

- Install dependencies:

  ```bash
  $ cd adatag
  $ npm i
  ```

- Create the required `.env` file:

  ```bash
  $ cat <<EOF> .env.custom
  NETWORK=Custom
  ENVIRONMENT=Integration
  PROVIDER=Kupmios
  EOF
  ```

- Create the required `API keys` and `secrets` file:

  ```bash
  $ cat <<EOF>  ./libs/common/src/config/keys/api-keys.json
  {
    "Blockfrost": {
      "Custom": "",
      "Preview": "",
      "Preprod": "",
      "Mainnet": ""
    },
    "Maestro": {
      "Custom": "",
      "Preview": "",
      "Preprod": "",
      "Mainnet": ""
    }
  }
  EOF

  $ cat <<EOF>  ./libs/common/src/config/keys/test-users-seed.json
  {
    "deployer": {
      "seed": "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo buddy"
    },
    "collector": {
      "seed": "ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice afford"
    },
    "user": {
      "seed": "test test test test test test test test test test test test test test test test test test test test test test test sauce"
    }
  }
  EOF
  ```

- Run the local demo:

  ```bash
  $ npx nx run @adatag/adatag.io:serve:custom
  ...
  > nx run @adatag/adatag.io:serve

  > vite serve

    VITE v5.2.10  ready in 397 ms

    ➜  Local:   http://localhost:4200/
    ➜  Press h + enter to show help
  ```

  The application will be available at http://localhost:4200/

  > Note: The command `npx nx run @adatag/adatag.io:serve-static:custom` will build and run the project in a production-like environment.

## User Roles

Three types of users are involved in the protocol:

1. Deployer: This user pootstraps the protocol.
2. User: Users who mint the @adatag and become the beneficiaries of the time-lock deposit (the rightful owners).
3. Collector: User who can collect unclaimed time-lock deposits (donations).

For testing or local demos, the following seed phrases and their corresponding addresses with a derivation path of 1852H/1815H/0H/0/0 are used:

- Deployer's Seed: `zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo buddy`
- Deployer's Address:`addr_test1qrp6j8zuzqazju9x9kqksrmlqguypd6ku6xqu75m99zf76c2g9x9fz9yhe8n5h9k2x6uvws7s5aqqwdmkk3clt93tjcqc2ljnk`

- User's Seed: `test test test test test test test test test test test test test test test test test test test test test test sauce`
- User's Address: `addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex`

- Collector's Seed: `ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice ice afford`
- Collector's Address: `addr_test1qzsk7aegh5rre3yhh5xl8r4k6vvkuqmf90fmfe9gkctu8tnpqamphkkru3r3p7va0yn0ws606fytvgq8gv4vaxekw3qs4r7hkk`

**DISCLAIMER**: These seeds should never be used on the mainnet as they could result in the loss of funds associated with these addresses.

# TODOs

## General

- [x] Test the integration with private Cardano blockchain
- [x] Create a deployment guide for users
- [ ] Address any security concerns or vulnerabilities
- [ ] Improve project's code quality and maintainability
- [ ] Investigate and implement potential performance optimizations
- [ ] Evaluate and consider integrating with other relevant projects or services
- [ ] Implement unit tests for critical functionalities
- [x] Document code architecture and design patterns used
- [x] Refactor codebase for improved readability and maintainability
- [x] Update project documentation
- [ ] Add a troubleshooting section to the documentation
- [ ] Write more unit tests
- [ ] Write integration tests for both off-chain and on-chain components
- [ ] Set up continuous integration (CI) for automated testing
- [x] Feature completeness

## Off-chain

- [ ] Clean up the codes
- [x] Implement user interface for username minting
- [x] Implement user interface for time-lock deposit claiming
- [ ] Implement user interface for username burning
- [x] Implement user interfaces for Lagal dosuments and FAQ
- [ ] Implement proper error handling
- [ ] Add more comprehensive logging
- [x] Refactor off-chain code for better modularity

## On-chain

- [ ] Reconsider deactivation feature
- [ ] Use Blake2b for hashing after Chang hard fork
- [ ] Review and update Plutus script validators
- [x] Optimize on-chain script performance
- [x] Add functionality to handle time-lock deposits for username minting
- [x] Add functionality to handle adahandle for avoiding time-lock deposits
- [ ] Add more test cases for on-chain scripts

# Contribution

Contributions to the @adatag IntegriTree PoC and MVP implementation are welcome! Whether it's bug fixes, feature enhancements, or documentation improvements, feel free to submit an [issue](https://github.com/ilap/adatag/issues) or [pull request](https://github.com/ilap/adatag/pulls) to help improve the project.

# License

This project is licensed under the [MIT License](LICENSE).
