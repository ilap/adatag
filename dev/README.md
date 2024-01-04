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

### Yaci-devkit's env file

```bash
topup_addresses=addr_test1qrqsm293uxd7zvs8yhaswenzzkkjxpfyfpaqufe0xjagp0hgyslwlf6ca9eend95lyw7pea32c2rtspq43sxd4a7sqwskerfjg:10000,addr_test1qrqsm293uxd7zvs8yhaswenzzkkjxpfyfpaqufe0xjagp0hgyslwlf6ca9eend95lyw7pea32c2rtspq43sxd4a7sqwskerfjg:500,addr_test1qrqsm293uxd7zvs8yhaswenzzkkjxpfyfpaqufe0xjagp0hgyslwlf6ca9eend95lyw7pea32c2rtspq43sxd4a7sqwskerfjg:50,addr_test1qp0ueqgz64d3vns8j2tp4ef8jh9dgq5qevhdrg2tz3vw0fnzl28slr3x8ngs8x72w3jgsgeympuscxfyzl53yd4k0cas4u67dp:10000,addr_test1qp0ueqgz64d3vns8j2tp4ef8jh9dgq5qevhdrg2tz3vw0fnzl28slr3x8ngs8x72w3jgsgeympuscxfyzl53yd4k0cas4u67dp:500,addr_test1qp0ueqgz64d3vns8j2tp4ef8jh9dgq5qevhdrg2tz3vw0fnzl28slr3x8ngs8x72w3jgsgeympuscxfyzl53yd4k0cas4u67dp:50,addr_test1qzza6achtargva760zfz8q37wfyl03sjgzev2rtsphew5r0qsh7mgst7chd99j6y6zqf00wx7whmydyjx2tqzxg0vv2qrn4s4m:10000,addr_test1qzza6achtargva760zfz8q37wfyl03sjgzev2rtsphew5r0qsh7mgst7chd99j6y6zqf00wx7whmydyjx2tqzxg0vv2qrn4s4m:500,addr_test1qzza6achtargva760zfz8q37wfyl03sjgzev2rtsphew5r0qsh7mgst7chd99j6y6zqf00wx7whmydyjx2tqzxg0vv2qrn4s4m:50```