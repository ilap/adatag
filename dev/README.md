# Development Environment Overview

The @adatag development environment integrates `Aiken` and `Plutus` (deprecated) for on-chain development, and the `Bun` for off-chain development.

In TypeScript-based dApp development on the off-chain side, the following components are used:

- Yaci-devkit: This tool offers a Cardano private network, enabling interactions similar to those on the mainnet. It proves especially valuable for integration and end-to-end tests, allowing the creation of scenarios that replicate real-world behavior in a private environment without interacting with the mainnet.

- TxPipe's Translucent (a Lucid fork with active development): Serving as a cruical component, it handles deployment and backend interactions with the Cardano mainnet, the private network provided by Yaci-devkit, or even a blockchain emulator. This component interacts with API providers such as blockfrost, maestro, or private instances of ogmios/kupo.

> Note: Yaci-Devkit relies on Docker, requiring the presence of a functioning Docker engine on the system.

## Prerequisites

- [Aiken](https://aiken-lang.org/installation-instructions)
- [Bun](https://bun.sh/docs/installation)
- [Yaci-Devkit](https://github.com/bloxbean/yaci-devkit)
- [Docker Engine](https://docs.docker.com/engine/install/)


An example install on Ubuntu:

```bash
# Dev
$ sudo apt-get update && sudo apt-get upgrade

# Clone the project
$ git clone https://gihub.com/ilap/adatag-dapp && cd adatag-dapp/dev
...

# Aiken
$ curl -sSfL https://install.aiken-lang.org | bash
...
$ source /home/ilap/.bashrc
$ aikup
...

# Bun
$ curl -fsSL https://bun.sh/install | bash 

# Docker
$ wget "https://desktop.docker.com/linux/main/amd64/docker-desktop-4.26.1-amd64.deb"
$ sudo apt install docker-docker-desktop-4.26.1-amd64.deb
...
$ git clone https://github.com/bloxbean/yaci-devkit
$ cd yaci-devkit
$ sudo ./start.sh
...
$ ./yaci-cli.sh
...
yaci-cli:>create-node -o --start
...
devnet:default>topup addr_test1qrqsm293uxd7zvs8yhaswenzzkkjxpfyfpaqufe0xjagp0hgyslwlf6ca9eend95lyw7pea32c2rtspq43sxd4a7sqwskerfjg 10000 # in ADA
...

```

As a bash script to install everything
```bash
```

## Initialise the development environment.

Create `.env` files for each environment e.g. `.env.local`, `.env.preview`, `.env.preprod` and `.env.mainnet`

The `.env` template

```bash
NETWORK="Preview"
PLUTUS_VERSION="PlutusV2"
# In V2 use "Blake2b_224" instead
HASH_ALG ="Sha2_256"

# In Ada
DEPOSIT_BASE=1750

# In days
LOCKING_DAYS=20

# In, days after 
DEACTIVATION_TIME=1
COLLECTION_TIME=2

# Only exists on mainnet.
# In development a very simple NFT minting policy is used for testing.
ADAHANDLE = ""

# Provider's settings for

# Currently the only valid options are:
# "KupmiosV5", "Kupmios", "Blockfrost" and "Maestro"
PROVIDER="KupmiosV5"

# Blockfrost
BLOCKFROST_URL="http://localhost:8080/api/v0"
BLOCKFROST_API_KEY="previewCQ....LF"

# Maestro
MAESTRO_API_KEY="CjL6...zhc"

# Kupo and Ogmios combo e.g. Yaci-devkit or similar
OGMIOS_URL="http://localhost:1337"
KUPO_URL="http://localhost:1442"

```