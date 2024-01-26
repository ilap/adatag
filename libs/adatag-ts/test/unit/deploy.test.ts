import { describe, test, expect } from 'bun:test'

import { Translucent } from 'translucent-cardano'

import { GenesisConfig, genesisParams } from '@adatag/shared/config'
import { Bootstrap } from '@adatag/shared/utils'

import {
  resolveMockData,
  setSloctConfig,
} from '@adatag/shared/test-utils'

/*
  https://github.com/input-output-hk/plutus-pioneer-program/blob/b55a7d2409cbf09a04ab471796f410083129acb6/code/Week03/lucid-ref-script/src/index.js

  Note: Topup the user's wallet
  Collector address : addr_test1qrqsm293uxd7zvs8yhaswenzzkkjxpfyfpaqufe0xjagp0hgyslwlf6ca9eend95lyw7pea32c2rtspq43sxd4a7sqwskerfjg
  Deployer address  : addr_test1qp0ueqgz64d3vns8j2tp4ef8jh9dgq5qevhdrg2tz3vw0fnzl28slr3x8ngs8x72w3jgsgeympuscxfyzl53yd4k0cas4u67dp
  Test User address : addr_test1qzza6achtargva760zfz8q37wfyl03sjgzev2rtsphew5r0qsh7mgst7chd99j6y6zqf00wx7whmydyjx2tqzxg0vv2qrn4s4m
*/
describe('Adatag minting', async () => {
  // The envs can be overwritten for dynamic testings, see and example below.
  /* eslint-disable-next-line @typescript-eslint/no-unused-vars */
  const { deployerSeed, collectorSeed, userSeed, network, provider } =
    await resolveMockData()

  setSloctConfig(network, Bun.env.ENVIRONMENT || '')
  
  const translucent = await Translucent.new(provider, network)
  
  // Select the receiving wallet.
  const collectorAddress = await translucent
    .selectWalletFromSeed(collectorSeed)
    .wallet.address()

  // Access the params object for the specified network
  const params = genesisParams[network]

  //let deployed = false
  let GenesisConfig: GenesisConfig

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
