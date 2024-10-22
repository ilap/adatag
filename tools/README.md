# Development Environment Overview

The @adatag development environment integrates `Aiken` and `Plutus` (deprecated) for on-chain development, and the `Typescript` for off-chain development.

In TypeScript-based dApp development on the off-chain side, the following components are used:

- Yaci-devkit: This tool offers a Cardano private network, enabling interactions similar to those on the mainnet. It proves especially valuable for integration and end-to-end tests, allowing the creation of scenarios that replicate real-world behavior in a private environment without interacting with the mainnet.

- TxPipe's Translucent (a Lucid fork with active development): It handles deployment and backend interactions with the Cardano mainnet, the private network provided by Yaci-devkit, or even a blockchain emulator. This component interacts with API providers such as blockfrost, maestro, or private instances of ogmios/kupo.

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
# Aiken v1.1.0 is not supported yet.
aikup install v1.0.29-alpha

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

## Environments

### Repository structure:

- The project, `@Adatag`, is hosted on GitHub as a monorepo.
- It utilises `NX` for effective management of a package-based monorepo structure with diverse packages such as TypeScript, Aiken, Plutus, Dart, etc., located under the `./apps`, `./lib` and `./contracts` directories.

### Development Workflow:

- Development is split into three main environments: Development, Integration, and Production.
- In the Development environment, all builds generate debug/trace outputs. On-chain/off-chain tests use local resources, such as the Plutus script running Emulator, providing mock API and mock blockchain functionalities.

### Integration Environment:

In the Integration environment, on-chain/off-chain dApp tests use real API providers with three types of blockchain endpoints:

1. Custom: A private network that can be easily spun up and turned down as a local blockchain cluster.
2. Preview Network: A public Cardano blockchain.
3. Preprod Network: Also a public network, similar to the mainnet configuration.

### Production Environment:

In the Production environment, all builds remove debug symbols (from app etc.) and traces from the compiled Plutus scripts.
This environment is meant for release, and no tests are executed directly here.
All tests must have passed in the Development and Integration stages.

### Configuration Overview:

Switching between environments, networks, and API providers is managed using specific `.env` files.

See the table below for the current configuration:
| Environment | Network | API Provider | Av. | Comment |
|------------- |---------- |--------------------- |----- |--------- |
| Development | Emulator | Emulator | Yes | |
| Integration | Custom | Kupmios | Yes | |
| Integration | Preview | Kupmios | Yes | using demeter.run |
| Integration | Preprod | Private Ogmios/Kupo | Yes | |
| Production | Mainnet | Private Ogmios/Kupo | Yes | |

## Integration Settings

### Custom Network using Yaci-Devkit

#### Requirements for Testing:

- Ensure proper funds and users are set up for testing.
- Update the `./dev/yaci-devkit/env` file with addresses for test users (deployer, collector, user).

#### Example Topup in env file

```
topup_addresses=<address:amount>,...,<address:amount>
```

#### Time Configuration:

- The dApp's on-chain validation requires proper validity range therefore, ensure the system start and slot duration are set.
- Modify yaci-devkit/ssh.sh for dynamic slot durations in Custom networks.

```diff
--- ./ssh.sh	2024-01-07 13:21:38
+++ ./ssh_old.sh	2024-01-07 13:22:45
@@ -5,5 +5,5 @@
    CMD="docker compose"
fi

-$CMD --env-file env  exec yaci-cli /bin/bash "$@"
+$CMD --env-file env  exec yaci-cli /bin/bash
```

```bash
./ssh.sh egrep -i "slot[dl]|start" /clusters/default/genesis/{byron,shelley}/genesis.json
```

#### Currently used topup addresses and amounts

```bash
topup_addresses=addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex:100000,addr_test1qrp6j8zuzqazju9x9kqksrmlqguypd6ku6xqu75m99zf76c2g9x9fz9yhe8n5h9k2x6uvws7s5aqqwdmkk3clt93tjcqc2ljnk:100000,addr_test1qzsk7aegh5rre3yhh5xl8r4k6vvkuqmf90fmfe9gkctu8tnpqamphkkru3r3p7va0yn0ws606fytvgq8gv4vaxekw3qs4r7hkk:100000
```

> Note: These adresses are based on the test users's (deployer, collector, user) seed. So, replace it accordingly.

### Low level cardano cli test

1. print out the valid tx with `toHex(signedTx.txSigned.to_bytes())`
2. log in to docker ./ssh.sh
3. create a babage tx with:

```bash
$ cat << EOF > signed.tx
{
    "type": "Witnessed Tx BabbageEra",
    "description": "Ledger Cddl Format",
    "cborHex":  "84..."
}
EOF
# Or Unwitnessed for non signed transaction
$ root@2e729ab2a6cf:/app# cardano-cli transaction view --output-json --tx-file signed.tx
$ root@2e729ab2a6cf:/app# cardano-cli transaction submit --testnet-magic 42 --socket-path /clusters/default/node-spo1/node.sock --tx-file signed.tx
...

```

## Troubleshooting

### Ogmios/Kupo does not work anymore with Yaci-Devkit in the latest Docker Deckstop on macOS (v4.29.0 (145265))

```bash
# Probably fixed by now.
cd /app
mkdir tmp && cd tmp
cp -pr ../yaci-cli.jar .

jar xf ../yaci-cli.jar BOOT-INF/classes/localcluster.zip
jar xf BOOT-INF/classes/localcluster.zip localcluster/templates/babbage/kupo.sh

sed -i 's/localhost/0.0.0.0/g' localcluster/templates/babbage/kupo.sh

jar uf BOOT-INF/classes/localcluster.zip localcluster/templates/babbage/kupo.sh
jar uf ../yaci-cli.jar BOOT-INF/classes/localcluster.zip
cd .. && rm -rf ./tmp
```

### Troubleshooting transaction

```bash
~./tools/chain/script/ssh.sh

# Or Unvitnessed...
echo '{
    "type": "Witnessed Tx ConwayEra",
    "description": "Ledger Cddl Format",
    "cborHex": "REPLACE ME WITH TX CBOR"
}' > test.tx

cardano-cli transaction view --output-json --tx-file test.tx
```
