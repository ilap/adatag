import { describe, test, expect } from 'bun:test'
import { Bootstrap } from '../../src/lib/bootstrap'
import { GenesisConfig, genesisParams } from '@adatag/common/config'
import { Blaze, Provider, Wallet, HotWallet, Data } from '@blaze-cardano/sdk'
import { resolveMockData, stringifyData, setSlotConfig, SLOT_CONFIG_NETWORK } from '@adatag/common/utils'

describe('Deployment test', async () => {
  const { deployerMasterkey, _, collectorMasterkey, network, provider } = await resolveMockData()

  setSlotConfig(network, Bun.env.ENVIRONMENT || '')
  console.log(`SLOT_CONFIG_NETWORK: ${JSON.stringify(SLOT_CONFIG_NETWORK[network])}`)

  const collectorAddress = (await HotWallet.fromMasterkey(collectorMasterkey, provider)).address

  // Access the params object for the specified network
  const params = await genesisParams[network]

  test('On-chain deployment', async () => {
    // Create a newParams object by spreading the values from the original params
    const testParams = {
      ...params,
      collectorAddress: collectorAddress.toBech32().toString(),
      collectionTime: 0,
      deactivationTime: 0,
      lockingDays: 0,
    }

    // Select spending wallet
    const deployerWallet = await HotWallet.fromMasterkey(deployerMasterkey!, provider)
    const [utxo] = await deployerWallet.getUnspentOutputs()

    console.log(`utxos: ${stringifyData(utxo.toCore())}`)

    const deployerBlaze: Blaze<Provider, Wallet> = await Blaze.from(provider, deployerWallet)
    const result = await Bootstrap.deploy(deployerBlaze, utxo, testParams)
    expect(result).toBeDefined()
  })
})
