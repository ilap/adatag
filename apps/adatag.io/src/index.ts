import * as Comlink from 'comlink'
import { debugMessage, setSloctConfig, stringifyData } from './utils.ts'
import { TreeWorkerService } from './workers/types.ts'
import { Kupmios, Translucent, Network, SLOT_CONFIG_NETWORK, toHex } from 'translucent-cardano'
import { KUPO_URL, OGMIOS_URL, NETWORK, ENV } from './configs/settings.ts'
import Config from './configs/genesis-config.json'
import { user as userSeed } from './configs/test-users-seed.json'
import { AdatagMintingService } from './services/MintingService.ts'

/* 
 adatag 
 nx run chain:spin-up:int
 cd libs/deployment-ts && bun test mock-adahandle adatag-minting --env-file ../../.env.test.int --timeout 60000
 cd ../../apps/fetch-test && bun run dev
 cd ../..
*/

// Setup the worker
const worker = new Worker(new URL('./workers/TreeWorker.ts', import.meta.url), {
  type: 'module',
})
const api = Comlink.wrap<TreeWorkerService>(worker)
const workerInstance = Comlink.proxy(api)

// Setup the minting service
const provider = new Kupmios(KUPO_URL, OGMIOS_URL)
await setSloctConfig(NETWORK, ENV)
console.warn(`SLOT CFG: ${stringifyData(SLOT_CONFIG_NETWORK)}`)
const translucent = await Translucent.new(provider, Config.network as Network)
translucent.selectWalletFromSeed(userSeed.seed)


const address = await translucent.wallet.address()
//const { paymentCredential } = this.translucent.utils.getAddressDetails(address!)

console.log(`@@@@@ ADDRESSS ${address}`)
const mintingService = new AdatagMintingService(translucent)


await workerInstance.initialise()
// Adatag to mint
// Has adahandle: "ada", "adika", "adam1997"
const adatag = 'adam0080'

// Confirmation timeout in seconds
const timeout = 30

// 1. search for adatag on the chain
// React: it will open a dialog
if (!(await workerInstance.checkIfAdatagMinted(adatag))) {
  debugMessage(`Not exist on chain, started minting: ${adatag}`)
  // React: Information Dialog calculates deposit
  /////////////////////////////////////////////////////////////////////////////
  //const handleUtxo = await mintingService.getHandleUtxo(adatag)
  //debugMessage(`Handle UTxO: ${handleUtxo}`)
  const deposit = AdatagMintingService.getMinDeposit(adatag)

  // React: Minting Dialog that show progress
  /////////////////////////////////////////////////////////////////////////////
  const { datum, redeemer } = await workerInstance.createMintingDetails(adatag)

  //translucent.selectWalletFromSeed(userSeed.seed)

  const baseTx = await mintingService.buildBaseTx(adatag, false, address, deposit)

  const finalisedTx = await mintingService.finaliseTx(
    baseTx,
    adatag,
    address,
    datum,
    redeemer,
  )

  //const completedTx = await mintingService.completeTx(finalisedTx)
  console.log(`AAAAAAA 1`)
  const completedTx = await finalisedTx.complete()

  console.log(`AAAAAAA 2`)
  const signedTx = await completedTx.sign().complete()
  console.log(`AAAAAAA 3`)
  
  console.log(toHex(signedTx.txSigned.to_bytes()))

  const txHash = await signedTx.submit()

  console.log(`@@ Submitted TX Hash: ${txHash}...`) // ${stringifyData(signedTx)}`)

} else {
  console.log(`#########: ON CHAIN ${adatag}`)
  worker.terminate()
}
