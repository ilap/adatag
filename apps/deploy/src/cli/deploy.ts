import { Translucent } from 'translucent-cardano'
import { GenesisConfig, genesisParams } from '@adatag/common/config'
import { Bootstrap } from '../lib/bootstrap'
import { setSloctConfig, resolveMockData, stringifyData } from '@adatag/common/utils'
import { IntegriTree } from '@adatag/integri-tree'

const { deployerSeed, collectorSeed, network, provider } = await resolveMockData()

// If we want tu use validity ranges for transactin with private networks that use dynamic
// startup time and slot length then we need to gather the proper parameters somehow.
// - "Custom" assuming a private network or Emulator.
// - "Preview", "Preprod" and "Mainnet" assuming the well-know parameters.
setSloctConfig(network, Bun.env.ENVIRONMENT || '')

const translucent = await Translucent.new(provider, network)

// Select the collector's address for redeem/claim
const collectorAddress = await translucent.selectWalletFromSeed(collectorSeed).wallet.address()

interface Dictionary {
  [key: string]: IntegriTree
}

// Create an empty dictionary object
const treeMap: Dictionary = {}

// Initialise the trees
for (let i = 'a'.charCodeAt(0); i <= 'z'.charCodeAt(0); i++) {
  const char = String.fromCharCode(i)
  treeMap[char] = IntegriTree.fromLetter(char)
}

// Create a newParams object by spreading the values from the original params
const useTimelock = Bun.env.USE_TIMELOCK == undefined || Bun.env.USE_TIMELOCK === 'true'

// Access the params object for the specified network
const params = genesisParams[network]

const finalParams = {
  ...params,
  collectorAddress: collectorAddress,
  collectionTime: useTimelock ? params.collectionTime : 0.0,
  deactivationTime: useTimelock ? params.deactivationTime : 0.0,
  lockingDays: useTimelock ? params.lockingDays : 0.0,
}

console.log(`PARAMS: ${stringifyData(finalParams)}`)

// Select deployer's wallet
translucent.selectWalletFromSeed(deployerSeed)
const [utxo] = await translucent.wallet.getUtxos()

// FIXME: const result = await Bootstrap.deploy(translucent, utxo, testParams)
console.log(`Genesis config (${network.toString()}) is saved to: ./config/genesis-config-${network.toString().toLowerCase()}.json`)
const result =
  Bun.env.ENVIRONMENT == 'Development'
    ? await Bootstrap.deploy(translucent, utxo, finalParams)
    : await Bootstrap.deployAndSave(
        `./config/genesis-config-${network.toString().toLowerCase()}.json`,
        translucent,
        utxo,
        finalParams
      )

const bd: GenesisConfig = result

console.log(`##### BD: ${stringifyData(bd)}`)
