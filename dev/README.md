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

An example install on Ubuntu 22.04

```bash
#!/bin/env bash

# Update the server
sudo apt-get update && apt-get upgrade
sudo apt install apt-transport-https ca-certificates curl software-properties-common

# Install the prerequisities (Aiken, Bun and Docker)
# Aiken
curl -sSfL https://install.aiken-lang.org | bash
source ~/.bashrc
aikup

# Bun
curl -fsSL https://bun.sh/install | bash 

# Docker
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

sudo apt update
apt-cache policy docker-ce
sudo apt install docker-ce
# Check it
sudo systemctl status docker

# Executing the Docker Command Without Sudo
# Open a new terminal after this to be applied
sudo usermod -aG docker ${USER}

# Clone the dApp
git clone git@github.com:ilap/adatag-dapp && cd adatag-dapp/dev

# Cllone the yaci-devkit
git clone https://github.com/bloxbean/yaci-devkit

# Reboot the system

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

# In, days after bootstrap (Mainnet default: 183, 365)
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