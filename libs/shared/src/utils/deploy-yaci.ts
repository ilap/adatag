import { Translucent } from 'translucent-cardano'
import * as GenesisParams from '../../configs/genesis-params.json'
import { ProviderFactory } from './provider-factory'
import { Bootstrap } from './bootstrap'
import * as Seeds from '../../keys/test-users-seed.json'
/*
  https://github.com/input-output-hk/plutus-pioneer-program/blob/b55a7d2409cbf09a04ab471796f410083129acb6/code/Week03/lucid-ref-script/src/index.js

*/
const env = 'Integration'
const network = 'Custom'
const provider = 'KupmiosV5'

/// Note: Topup the user's wallet
// Collector address : addr_test1qrqsm293uxd7zvs8yhaswenzzkkjxpfyfpaqufe0xjagp0hgyslwlf6ca9eend95lyw7pea32c2rtspq43sxd4a7sqwskerfjg
// Deployer address  : addr_test1qp0ueqgz64d3vns8j2tp4ef8jh9dgq5qevhdrg2tz3vw0fnzl28slr3x8ngs8x72w3jgsgeympuscxfyzl53yd4k0cas4u67dp
// Test User address : addr_test1qzza6achtargva760zfz8q37wfyl03sjgzev2rtsphew5r0qsh7mgst7chd99j6y6zqf00wx7whmydyjx2tqzxg0vv2qrn4s4m
const pr = ProviderFactory.createProvider(env, network, provider)
const translucent = await Translucent.new(pr, network)

/// Retrieve the users's seed
translucent.selectWalletFromSeed(Seeds.deployer.seed)
const collectorAddress = await translucent.wallet.address()

translucent.selectWalletFromSeed(Seeds.deployer.seed)
const [utxo] = await translucent.wallet.getUtxos()

// Access the params object for the specified network
const params = GenesisParams[network]

// Change the default colllector address.
const testParams = {
  ...params,
  collectorAddress: collectorAddress,
}

const plutus = await Bootstrap.deployAndSave(
  './config/app-yaci-config.json',
  translucent,
  utxo,
  testParams,
)

console.log(JSON.stringify(plutus, null, '  '))
