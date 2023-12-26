# @adatag dApp

Clone the adatag dApp repository

```bash
$ git clone https://github.com/ilap/adatag-dapp
$ cd adatag-dapp
```

## Bootstrapping the dApp

Bootstrapping refers to configuring, deploying, and making all on-chain components ready for use by any off-chain client.

The initial step involves compiling and building the validator- and minting policy scripts. Following this, apply the manually adjusted parameters to the dApp's parameterized validators and minting policy scripts to obtain the final script hashes and/or their addresses (for validator scripts) and currency symbols (for minting policy scripts). This information is crucial for deploying the final scripts on-chain.

The dApp deployment consists of the following steps:

1. Create the 26 authorization tokens.
2. Send these authorized tokens, along with an initial state (as an inline datum), to the state holder validator's address, resulting in 26 UTXOs.
3. Send all three (TimeLockDeposit, StateHolder, and AdatagMinting) properly configured scripts to the AlwaysFail validator's address as reference scripts. This results in three immutable UTXOs (non-spendable), ensuring that these scripts are always available.

After deploying the scripts to the chain (initially to the Preview), the dApp can be tested using various off-chain tools (cardano-cli, developed frontend, etc.).

### Build the scripts

```bash
$ cd on-chain/aiken
# It generates the `plutus.json` blueprint for the dApp
$ aiken build
$ cd ../../off-chain
```

### Adjusting the dApp parameters

Amend the dApp's parameters:
```bash
$ vi ./config/constants.ts
```

```typescript
export const NETWORK = "Preview"
export const BOOTSTRAP_TIME = Date.now()
export const DEPOSIT_BASE = 1750
export const SECRETS_DIR = "./secrets/"
export const COLLECTOR_KEY = SECRETS_DIR + "collector.key"
export const COLLECTOR_SEED = SECRETS_DIR + "collector.seed"
export const COLLECTOR_ADDR = SECRETS_DIR + "collector.addr"
export const COLLECTION_TIME = BOOTSTRAP_TIME + days(2)
export const LOCKING_DAYS = days(20)
export const DEACTIVATION_TIME = BOOTSTRAP_TIME + days(1)
// Only exists on mainnet, for development 
// create some very simple NFT minting policy that everybody can mint burn 
export const ADAHANDLE = "010203040506070809"
```


#### Generate the required credential

The time lock deposit script needs a collector address to collect donations and unclaimed deposits.

```bash
$ bun install
# Generate the required credentials for bootsrapping  
$ bun run scripts/generate-keys.ts
Network          : Preview
Collector address: addr_test1qrqsm293uxd7zvs8yhaswenzzkkjxpfyfpaqufe0xjagp0hgyslwlf6ca9eend95lyw7pea32c2rtspq43sxd4a7sqwskerfjg
Faucet address   : https://docs.cardano.org/cardano-testnet/tools/faucet/
```

### Deploying the dApp

This final step involves generating the final scripts by applying parameters, and then deploying them onto the Cardano blockchain as part of a single transaction.

The transaction encompasses a collateral and a normal input, the minting policy for auth tokens, the three scripts designated for the AlwaysFail validator's address as reference scripts, and the required 29 ouptus for deployment (see the table below) along with an additional output for handling changes.

| Outputs 	| Address             	| Value         	| Datum          	| Ref Script      	| Comment 	|
|------	|---------------------	|---------------	|----------------	|-----------------	|---------	|
| 1    	| AlwaysFail address  	| Lovelace      	| NoDatum        	| StateHolder     	|         	|
| 2    	| AlwaysFail address  	| Lovelace      	| NoDatum        	| AdatagMinting   	|         	|
| 3    	| AlwaysFail address  	| Lovelace      	| NoDatum        	| TimeLockDeposit 	|         	|
| 4    	| StateHolder address 	| AuthToken."a" 	| InitialState_a 	| N/A             	|         	|
| ...  	| ...                 	| ...           	| ...            	| ...             	|         	|
| 29   	| StateHolder address 	| AuthToken."z" 	| InitialState_z 	| N/A             	|         	|


```bash
$ bun run scripts/deploy.ts
Collector pkh       : c10da8b1e19be1320725fb07666215ad230524487a0e272f34ba80be
UTxO                : 10000
Auth Token symbol   : cb494decd037ffa073cc2705a1b83b547ef749975e5616441256ff7b
Ref script address  : addr_test1wpn2vamfahgv2n2ldmyt5wf9899lpe8ur79lhcapx844y0qrnvrgh
State holder address: addr_test1wram9hmcvhsgwr58g988u39aythq89wrsjqdjwhh7478tzqw3xase
Timelock script hash: 1b03873373c39eb12b8c3af58140570fa31cbb7ab67b401c7726f774
Timelock address    : addr_test1wqds8penw0peavft3sa0tq2q2u86x89m02m8ksquwun0waqaqhjns
Minting hash        : f8b6e2c96e396fd9bec4d416dc978cd6bffb8c4b26bf7442e1ab5116
```
