import { Assets, Translucent, generateSeedPhrase } from 'translucent-cardano'
import * as PlutusParams from '../config/plutus-params.json'
import { ProviderFactory } from '../common/provider-factory.ts'
import { Bootstrap } from './bootstrap.ts'

/*
  The Bootstrap process requires the following steps:

Prerequisites (for tests, parameters can be programmatically changed; for production, the collector address and deployer UTxO must be manually set):
  1. Amend plutus parameters if necessary (plutus-params.json).
  2. Allocate a deployer UTxO with sufficient funds for deploying the required plutus scripts on-chain.
     - Top up at least 50 ADA/tADA for deployment.

Deploy Plutus scripts on-chain (applicable even in the emulator):
  1. Generate all plutus scripts based on Aiken's compiled code and plutus parameters.
  2. Create the genesis transaction for the dApp.
  3. Sign and submit the transaction.
  4. Save the deployment details into the config/app-config.json file (essential for building the release of the Web/UI app).

*/

async function generateAccount(assets: Assets) {
  const seedPhrase = generateSeedPhrase()
  return {
    seedPhrase,
    address: await (await Translucent.new(undefined, 'Custom'))
      .selectWalletFromSeed(seedPhrase)
      .wallet.address(),
    assets,
  }
}

const OWNER = await generateAccount({ lovelace: 75_000_000_000n })
const COLLECTOR = await generateAccount({ lovelace: 100_000_000n })
const USER = await generateAccount({ lovelace: 100_000_000n })

const env = 'Development'
const network = 'Custom'
const provider = 'Emulator'

const pr = ProviderFactory.createProvider(env, network, provider, [
  OWNER,
  COLLECTOR,
  USER,
])

// The transaction is built, but signed and submitted.

const translucent = await Translucent.new(pr)

translucent.selectWalletFromSeed(OWNER.seedPhrase)
const [utxo] = await translucent.wallet.getUtxos()

//const { paymentCredential } = getAddressDetails(COLLECTOR.address)

// Access the params object for the specified network
const params = PlutusParams[network]

// Create a newParams object by spreading the values from the original params
const testParams = {
  ...params,
  collectorAddress: COLLECTOR.address,
}

const plutus = await Bootstrap.deployAndSave(
  '../config/app-emulator-config.json',
  translucent,
  utxo,
  testParams,
)

console.log(JSON.stringify(plutus, null, '  '))
