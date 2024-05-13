<div align="center">
  <h1 align="center">@Adatag</h1>
    <h2 align="center" style="border-bottom: none">A unique username for everyone.</h2>

[![License](https://img.shields.io/github/license/ilap/adatag)](https://github.com/ilap/adatag/blob/main/LICENSE)

</div>

# Introduction
Welcome to **@Adatag**, a web3 username service built on the Cardano blockchain that enables users to manage unique, NFT-based usernames.
It utilizes [IntegriTree](#integritree) to ensure the uniqueness and integrity of usernames.

# Adatag Features

- **Decentralized**: @Adatag is a ***real*** dApp, not controlled by any single entity or keys.
- **Transparent**: @Adatag is completely open-source to allow anybody to validate and audit its logic.
- **Free**: @Adatag username creation (minting) and deletion (burning TBD) are completely free, with no price or royalty fees.
- **Instant**: @Adatag usernames - if available - can be created and deleted instantly, without any third-party involvement.


> Note: Users must time-lock deposit a certain value (depending on the length of the @Adatag) to prevent abuse of the system. These deposits are redeemable by the rightful beneficiaries after a certain time (preferably 20 days) of minting.

# IntegriTree

IntegriTree is a complete-binary tree-based data structure that stores only unique elements using open-interval, featuring a short accumulator (hash of the tree) and proofs (member, non-member, addition, and deletion proofs) that can be stored on-chain, similar to Merkle Tree, Trie, or Patricia Trees proofs.

# How @Adatag Works

Using @Adatag involves bootstrapping, which includes generating the required Plutus scripts based on the protocol parameters and deploying them onto the Cardano blockchain along with the initial state(s) of the protocol.

- Protocol State:  Initially, the protocol state is based on the relevant trees containing only one initial node that includes the lower and upper bounds of the open interval (e.g., like a `[-Infinity, +Infinity]` open interval). This initial bootstrap is crucial for ensuring the integrity of the system.
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

> Note: These dependencies are essential for setting up and running @Adatag effectively.

# Local Demo for @Adatag

To demo @Adatag on a custom private Cardano blockchain using Yaci Devkit, follow these steps (assuming all prerequisites are completed):

1. Custom Nami Wallet: Since Yaci Devkit does not fully support Blockfrost API, a custom-built Nami wallet is required for the demo.
  - Clone the repository: 
    ``` bash 
    git clone http://github.com/ilap/nami
    ```
  - Navigate to the Nami's directory:
    ``` bash
    cd nami
    ```

  - Copy the testing secrets file:
    ``` bash 
    cp secrets.testing.js secrets.production.js
    ```

  - Install dependencies and build the wallet (Nami requires Node.js v20):
    ``` bash 
    npm i && npm run build
    ```
  - Add the custom Nami wallet to Chrome/Brave extensions:
    - Open the extensions page: `brave://extensions/` or `chrome://extensions/`
    - Enable "Developer mode" by toggling the switch in the top-right corner
    - Click "Load unpacked" and select the `./build` directory from the Nami directory.

  - Create a wallet using the following seed:
  ` test test test test test test test test test test test test test test test test test test test test test test test sauce`


2. Build and run @Adatag on custom private Cardano blockhain:

```bash
$ git clone https://github.com/ilap/adatag
$ cd adatag
$ npm i
$ cat <<EOF> .env.custom 
NETWORK=Custom
ENVIRONMENT=Integration
PROVIDER=Kupmios
EOF

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
    "seed": "test test test test test test test test test test test test test test test test test test test test test test test sauce"
  },
  "collector": {
    "seed": "test test test test test test test test test test test test test test test test test test test test test test test sauce"
  },
  "user": {
    "seed": "test test test test test test test test test test test test test test test test test test test test test test test sauce"
  }
}
EOF

```

The application will be available at http://localhost:4200/.

``` bash 
$ npx nx run @adatag/adatag.io:serve:custom
...
> nx run @adatag/adatag.io:serve

> vite serve

  VITE v5.2.10  ready in 397 ms

  ➜  Local:   http://localhost:4200/
  ➜  Press h + enter to show help
```



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
