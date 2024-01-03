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
$ vi ./config/plutus-params.json
```

```json
{
  "Custom": {
    "hashAlg": "Sha2_256",
    "collectorAddress": "addr_test1qrqsm293uxd7zvs8yhaswenzzkkjxpfyfpaqufe0xjagp0hgyslwlf6ca9eend95lyw7pea32c2rtspq43sxd4a7sqwskerfjg",
    "collectionTime": 0.08,
    "depositBase": 1750,
    "deactivationTime": 0.04,
    "lockingDays": 0.02,
    "adahandle": "d8906ca5c7ba124a0407a32dab37b2c82b13b3dcd9111e42940dcea4"
  },
  "Preview": {
    "hashAlg": "Sha2_256",
    "collectorAddress": "addr_test1qrqsm293uxd7zvs8yhaswenzzkkjxpfyfpaqufe0xjagp0hgyslwlf6ca9eend95lyw7pea32c2rtspq43sxd4a7sqwskerfjg",
    "collectionTime": 2,
    "depositBase": 1750,
    "deactivationTime": 1,
    "lockingDays": 0.04,
    "adahandle": "f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a "
  },
  "Preprod": {
    "hashAlg": "Blake2b_224",
    "collectorAddress": "addr_test1qrqsm293uxd7zvs8yhaswenzzkkjxpfyfpaqufe0xjagp0hgyslwlf6ca9eend95lyw7pea32c2rtspq43sxd4a7sqwskerfjg",
    "collectionTime": 365,
    "depositBase": 1750,
    "deactivationTime": 183,
    "lockingDays": 20,
    "adahandle": "f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a "
  },
  "Mainnet": {
    "hashAlg": "Blake2b_224",
    "collectorAddress": "addr1q8qsm293uxd7zvs8yhaswenzzkkjxpfyfpaqufe0xjagp0hgyslwlf6ca9eend95lyw7pea32c2rtspq43sxd4a7sqws407f7h",
    "collectionTime": 365,
    "depositBase": 1750,
    "deactivationTime": 183,
    "lockingDays": 20,
    "adahandle": "f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a"
  }
}
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

| Outputs | Address             | Value         | Datum          | Ref Script      | Comment |
| ------- | ------------------- | ------------- | -------------- | --------------- | ------- |
| 1       | AlwaysFail address  | Lovelace      | NoDatum        | StateHolder     |         |
| 2       | AlwaysFail address  | Lovelace      | NoDatum        | AdatagMinting   |         |
| 3       | AlwaysFail address  | Lovelace      | NoDatum        | TimeLockDeposit |         |
| 4       | StateHolder address | AuthToken."a" | InitialState_a | N/A             |         |
| ...     | ...                 | ...           | ...            | ...             |         |
| 29      | StateHolder address | AuthToken."z" | InitialState_z | N/A             |         |

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

## Local Development Environment

Unit & Property tests - Aiken check (Unit tests only) - Translucent emulator (Unit and property tests)

Integration tests - Private Cardano Network with (Ogmios, Meastro, BlockFrost, Koios)

Acceptance tests - Preview Deployment with different providers (Ogmios/Kupo, Maestro, Blockfrost, Koios) - Preprod Deployment with the selected provider - Mainnet Deployment with the selected provider

## Deployment information

Deployment details
-- Public Informations after deployment:

Network: Preprod

Reference script detail:
Reference Adress
Stateholder: txhHash, index
MintingPolicy: txhHash, index
Timelock script: txHash, index

Minting Details:

1. References AlwaysFail validator

- Source code (Plus info to regenerate hash/address)
- Validator hash
- Validatopr Address

3. Authorization one-shot token minting script.

- Source code (Plus info to regenerate hash/address)
- params: Owner txHash, outputIndex.
- mintingPolicy (Symbol) ID.
- Oneshot minting Transaction link.

4. StateHolder:

- Source code (Plus info to regenerate hash/address)
- params: Auth NFT Symbol,
- stateHolder address
- stateHolder hash
- Reference script link

5. Adatag Minting:

- Source code (Plus info to regenerate hash/address)
- params:
  - auth token symbol,
    - stateholder hash,
    - timelock hash,
    - deactivation time: posix and real time
    - deposit base:
    - locking days:
    - adahandle:
- stateHolder address
- Reference script link

Collection Details

1. TimeLock Validator Script

- Params: collector PkH, collection_time: in Posix.
- Validator Hash
- Validator source.

### Minting/Burning and Collection/Claim

[Examples](https://github.com/input-output-hk/plutus-pioneer-program/blob/b55a7d2409cbf09a04ab471796f410083129acb6/code/Week03/lucid-ref-script/src/index.js)

#### Minting/Burning

Claiming is about find utxo in which the beneficiary pkh equals with our key.
It does not depend on the script parameters only the datum values (beneficiary, deadline)
Required variables, parameters:

1. Adatag: user input
2. genesis transaction: deployData.genesis_transaction
3. Stateholder ref index: deployData.stateholder.reference_index
4. Statholder address: deployData.stateholder.address
5. Mintingpolicy ref index: deployData.adatag_minting.reference_index
   // 6. Mintingpolicy id: from adatag.json
   // 7. TimelockDeposit ref index: from adatag.json
6. TimelockDeposit adress: deployData.timelock.address
7. Auth token symbol: deployData.auth_minting.symbol
8. Auth token name: adatag[0]
9. Proof: from old state (utxo's datum).
10. Mintvalue: from Adatag and tree (+/- 1).
11. New state: from tree + adatag + action

12. deadline: deployData.adatag_minting.params.deadline
13. deactivation time: deployData.adatag_minting.params.deactivation_time
14. adahandle: deployData.adatag_minting.params.adahandle
15. deposit: calcDeposit(adatag, deployData.adatag_minting.params.deposit_base)

```typescript
const token_name = adatag[0]

const shref = translucent.utxosByOutRef([{
  txHash: deploy.genesis_transaction,
  outputIndex: deploy.stateholder.reference_index,
}]);

const mpref = translucent.utxosByOutRef([{
  txHash: deploy.genesis_transaction,
  outputIndex: deploy.adatag_minting.reference_index,
}]);

const auth_utxo = findAuthToken(
  deploy.stateholder.address,
  deploy.auth_minting.symbol,
  token_name,
)

if (! timeHasReached(deploy.adatag_minting.params.deactivation_time)) {
  const deposit = calcDeposit(adatag, deploy.adatag_minting.params.deposit_base)
  const deadline = Date.now() + daysInMs(deploy.adatag_minting.params.locking_days)
  const adahandleUtxo = findWalletUtxosWithValue(translucent, deploy.adatag_minting.params.adahandle, adatag)
}

const mp = deploy.adatag_minting.symbol

const (un, un1, an, proof, newTree) = mkProof(tree, adatag)

const (action, mintvalue) = un1 ? ("AdatagAdded", 1) : ("AdatagRemoved", -1)

const rh = root_hash(newTree)
const new_state = {
  operationCount: old_state.opcount + mintvalue
  adatag: adatag,
  operation: action,
  size: newTree.size
  rootHash: rh,
  mintingPolicy: mp
}

const tx = translucent
  .newTx()
  .readFrom([mpref, shref]) // (2, 3, 5) Na any attach is required then.
  .collectFrom([auth_utxo]) // (4,9,10) auth_utxo at stateholder address
  // Optional when deactivation time (16) is not reached
  .paytoContract(timelockdeposit, {deposit}, {benef, now+deadline}, noscript) // (8, 15,18) Optional, not required after deactivation time.
  .collectFrom([adahandle_utxo]) // (17)
  .payToAddress(user_addres, {adahandle},) (18)
  //
  .paytocontract(stateholder, [auth_symbol + fromText(token_name)], new_state, noscript) //(4, 9, 10, )
  .payToAddress(useraddres, {adatag}, {}, {}) (1)
  .mintasset( [mp + fromText(adatag)]: mintvalue, redeem) (12)
  // (14, ) It's not required, as we cannot prevent malicios client SDK based wallet to do anythuing
  // E.g. adding their own input and signing key to set the beneficiary for themselves.
  .addSigner(pkh)
```

#### Claiming

Claiming is about find utxo in which the beneficiary pkh equals with our key.
It does not depend on the script parameters only the datum values (beneficiary, deadline)
Required variables, parameters:

1. genesis transaction: deployData.genesis_transaction
2. timelock ref: deployData.timelock.reference_index
3. timelock address: deployData.timelock.address

```typescript
// Should we add adatag to find our utxos easier?
const utxos = findBeneficiaryUtxos(deploy.timelock.address, pkh, Date.now());
// All utxos have a valid datum thereofre deadline
const earliestDeadline = utxos.arr.reduce(function (utxo, v) {
  let deadline = utxo.datum.deadline;
  return deadline < v ? deadline : v;
});
const refUtxo = translucent.utxosByOutRef([
  {
    txHash: deploy.genesis_transaction,
    outputIndex: deploy.timelock.reference_index,
  },
]);

const tx = translucent
  .newTx()
  // timelock script (1,2)
  // genesis_transacion#2
  .readFrom([refUtxo])
  // (3,4,5) the timelock UTXO
  // it can home multiple claims, but all of the utxo's deadline must ha passed
  // their deadline.
  .collectFrom([utxos], "Redeem")
  // beneficiary. (pkh === beneficiary) from previous search
  .addSignerKey(pkh)
  // Important (5)
  // select the min from all utxos' datum.deadline
  // to pass. Also it must in the past.
  .validFrom(Number(earliestDeadline))
  .complete();
```

#### Collect

It only can be run after the collection time has reached.
Required variables, parameters:

1. genesis transaction: deployData.genesis_transaction
2. timelock ref: deployData.timelock.reference_index
3. collection time: deploydata.timelock.params.collection_time
4. collector Pkh: deploydata.timelock.params.collector_pkh

```typescript
const refUtxo = translucent.utxosByOutRef([
  {
    txHash: deploy.genesis_transaction,
    outputIndex: deploy.timelock.reference_index,
  },
]);

// Reduce the nr. of utxos to fit in a tx.
// It's caclulated that about 80 utxos could be Phase 2 validated in a tx.
const utxos = reduceUtxosToProperSeize(utxosAt(timeDepositAddress));

const tx = translucent
  .newTx()
  .readFrom([refUtxo]) // timelock script (1,2)
  // Subset of all unclaimed time-lock-deposits and donations.
  .collectFrom(utxos, "Collect")
  // We must have the collector's sk to be able to add it as a signer
  .addSignerKey(collectorPkh)
  .complete();
```
