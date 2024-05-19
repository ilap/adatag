import { describe, test, expect } from 'bun:test'

import { Translucent } from 'translucent-cardano'

import { genesisParams } from '@adatag/common/config'
import { Bootstrap } from '../../src/lib/bootstrap'

import { resolveMockData, setSloctConfig } from '@adatag/common/utils'

describe('Deployment test', async () => {
  // The envs can be overwritten for dynamic testings, see an example below.
  /* eslint-disable-next-line @typescript-eslint/no-unused-vars */
  const { deployerSeed, collectorSeed, network, provider } = await resolveMockData()

  setSloctConfig(network, Bun.env.ENVIRONMENT || '')

  const translucent = await Translucent.new(provider, network)

  // Select the receiving wallet.
  const collectorAddress = await translucent.selectWalletFromSeed(collectorSeed).wallet.address()

  // Access the params object for the specified network
  const params = genesisParams[network]

  test('On-chain deployment', async () => {
    // Create a newParams object by spreading the values from the original params
    const testParams = {
      ...params,
      collectorAddress: collectorAddress,
      collectionTime: 0,
      deactivationTime: 0,
      lockingDays: 0,
    }

    // Select spending wallet
    translucent.selectWalletFromSeed(deployerSeed)
    const [utxo] = await translucent.wallet.getUtxos()

    const result = await Bootstrap.deploy(translucent, utxo, testParams)
    expect(result).toBeDefined()
  })
})
