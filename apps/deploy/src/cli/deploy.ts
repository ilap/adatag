import { GenesisConfig, genesisParams } from '@adatag/common/config'
import { Bootstrap } from '../lib/bootstrap'
import { setSlotConfig, stringifyData, resolveMockData, mintAdahandle } from '@adatag/common/utils'
import { IntegriTree } from '@adatag/integri-tree'

import { Blaze, Provider, HotWallet, Wallet } from '@blaze-cardano/sdk'
import { Address } from '@blaze-cardano/core'

const delay = (ms: number) => new Promise(resolve => setTimeout(resolve, ms))

export async function mintMockAdahandle(deployerBlaze: Blaze<Provider, Wallet>, address: Address) {
  console.log(`Minting mock-adahandles.....`)
  delay(5000)
  mintAdahandle(deployerBlaze, ['ilap', 'pal', 'ada'], address)
}

// The envs can be overwritten for dynamic testings, see and example below.
// When manually set then all tests will use the owerwrittedn values.
//Bun.env.ENVIRONMENT = "Integration" //"Development" // "Production";
//Bun.env.NETWORK = "Custom";
//Bun.env.PROVIDER = "KupmiosV5" //"Emulator" // "KupmiosV5";
const { deployerMasterkey, collectorMasterkey, userMasterkey, network, provider } = await resolveMockData()

// If we want tu use validity ranges for transactin with private networks that use dynamic
// startup time and slot length then we need to gather the proper parameters somehow.
// - "Custom" assuming a private network or Emulator.
// - "Preview", "Preprod" and "Mainnet" assuming the well-know parameters.
setSlotConfig(network, Bun.env.ENVIRONMENT || '')

const collectorWallet = await HotWallet.fromMasterkey(collectorMasterkey!, provider)
const collectorAddress = collectorWallet.address

const userWallet = await HotWallet.fromMasterkey(userMasterkey!, provider)
const userBlaze = await Blaze.from(provider as Provider, userWallet)
const userAddress = userWallet.address

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
const useMockCollector = Bun.env.NETWORK === 'Custom'

// Access the params object for the specified network
const params = genesisParams[network]

const finalParams = {
  ...params,
  collectorAddress: collectorAddress.toBech32().toString(),
  collectionTime: useTimelock ? params.collectionTime : 0.0,
  deactivationTime: useTimelock ? params.deactivationTime : 0.0,
  lockingDays: useTimelock ? params.lockingDays : 0.0,
}

console.log(`PARAMS: ${stringifyData(finalParams)}`)

const deployerWallet = await HotWallet.fromMasterkey(deployerMasterkey!, provider)
const deployerBlaze = await Blaze.from(provider as Provider, deployerWallet)

// Select spending wallet
const [utxo] = await deployerWallet.getUnspentOutputs()

console.log(
  `Genesis config (${network.toString()}) is being saved to: ./config/genesis-config-${network.toString().toLowerCase()}.json`
)
const result =
  Bun.env.ENVIRONMENT == 'Development'
    ? await Bootstrap.deploy(deployerBlaze, utxo, finalParams)
    : await Bootstrap.deployAndSave(
        `./config/genesis-config-${network.toString().toLowerCase()}.json`,
        deployerBlaze,
        utxo,
        finalParams
      )

const bd: GenesisConfig = result

console.log(`##### BD: ${stringifyData(bd)}`)

if (Bun.env.NETWORK === 'Custom') {
  // Private network: imitate adahandle
  await mintMockAdahandle(userBlaze, userAddress)
}
