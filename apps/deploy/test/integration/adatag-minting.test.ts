import { describe, test, expect } from 'bun:test'
import {
  Data,
  Emulator,
  Translucent,
  fromText,
  toText,
  AddressDetails
} from 'translucent-cardano'
import * as P from  '@adatag/common/plutus'
import { GenesisConfig, genesisParams } from '@adatag/common/config'
import { Bootstrap } from '../../src/lib/bootstrap'
import {
  setSloctConfig,
  generateRandomStrings,
  isValidUsername,
  isLowerCase,
  calculateDeposit,
  stringifyData,
  resolveMockData
} from '@adatag/common/utils'
import { IntegriTree } from '@adatag/integri-tree'


//import { stringifyData } from '../utils/utils'

/*
  https://github.com/input-output-hk/plutus-pioneer-program/blob/b55a7d2409cbf09a04ab471796f410083129acb6/code/Week03/lucid-ref-script/src/index.js

  Note: Topup the user's wallet
  Collector address : addr_test1qrqsm293uxd7zvs8yhaswenzzkkjxpfyfpaqufe0xjagp0hgyslwlf6ca9eend95lyw7pea32c2rtspq43sxd4a7sqwskerfjg
  Deployer address  : addr_test1qp0ueqgz64d3vns8j2tp4ef8jh9dgq5qevhdrg2tz3vw0fnzl28slr3x8ngs8x72w3jgsgeympuscxfyzl53yd4k0cas4u67dp
  Test User address : addr_test1qzza6achtargva760zfz8q37wfyl03sjgzev2rtsphew5r0qsh7mgst7chd99j6y6zqf00wx7whmydyjx2tqzxg0vv2qrn4s4m
*/

// FIXME: only "a"'s and  
const prefix = "a" // or null

// It OOMs over 2K elements.
const mintNr = 12000

const denominator = prefix ? 10 : 100
const nr = Bun.env.ENVIRONMENT == 'Development' ? mintNr : Math.floor(mintNr / denominator)

// 16 is ok but we want to have some long name fails too.
const adatagLength = 18

// it could generate duplicates

const adatags = generateRandomStrings(nr, adatagLength, prefix)

// real nr. of elements.
const elemsNumber = adatags.size


describe(`Adatag minting (${elemsNumber})`, async () => {
  // The envs can be overwritten for dynamic testings, see and example below.
  // When manually set then all tests will use the owerwrittedn values.
  //Bun.env.ENVIRONMENT = "Integration" //"Development" // "Production";
  //Bun.env.NETWORK = "Custom";
  //Bun.env.PROVIDER = "KupmiosV5" //"Emulator" // "KupmiosV5";
  const { deployerSeed, collectorSeed, userSeed, network, provider } =
    await resolveMockData()

  // If we want tu use validity ranges for transactin with private networks that use dynamic
  // startup time and slot length then we need to gather the proper parameters somehow.
  // - "Custom" assuming a private network or Emulator.
  // - "Preview", "Preprod" and "Mainnet" assuming the well-know parameters.
  setSloctConfig(network, Bun.env.ENVIRONMENT || '')

  const translucent = await Translucent.new(provider, network)

  const userAddress = await translucent
    .selectWalletFromSeed(userSeed)
    .wallet.address()

  // Select the receiving wallet.
  const collectorAddress = await translucent
    .selectWalletFromSeed(collectorSeed)
    .wallet.address()

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

const testParams = {
  ...params,
  collectorAddress: collectorAddress,
  collectionTime: useTimelock ? params.collectionTime : 0.0,
  deactivationTime: useTimelock ? params.deactivationTime : 0.0,
  lockingDays: useTimelock ? params.lockingDays : 0.0,
}

console.log(`PARAMS: ${stringifyData(testParams)}`)

  // Select spending wallet
  translucent.selectWalletFromSeed(deployerSeed)
  const [utxo] = await translucent.wallet.getUtxos()

  // FIXME: const result = await Bootstrap.deploy(translucent, utxo, testParams)
  console.log(`#########  saveTo ./config/genesis-config-${network.toString().toLowerCase()}.json`)
  const result = Bun.env.ENVIRONMENT == 'Development' ?
    await Bootstrap.deploy(translucent, utxo, testParams)
    : await Bootstrap.deployAndSave(`./config/genesis-config-${network.toString().toLowerCase()}.json`, translucent, utxo, testParams)


  const bd: GenesisConfig = result

  // Set the minting- and state-hodler policies' reference UTxOs
  // TODO: Find some better method to fetch these static references from chain.
  const refUtxos = await translucent!.utxosByOutRef([
    // get the state holder reference
    {
      txHash: bd.genesisTransaction,
      outputIndex: bd.stateholderScript.refIndex,
    },
    // get the minting policy reference
    {
      txHash: bd.genesisTransaction,
      outputIndex: bd.adatagMinting.refIndex,
    }
  ])

  const action: P.Operation = 'AdatagAdded'
  const mintAmount: bigint = 1n
  const mp = bd.adatagMinting.policyId

  let i = 0

  for (const adatag of adatags) {
    if (translucent.provider instanceof Emulator) {
      translucent.provider.awaitBlock(10)
    }

    const authTokenName = isLowerCase(adatag.charCodeAt(0))
      ? adatag[0]
      : String.fromCharCode(Math.floor(Math.random() * (122 - 97) + 97))
    const authUnit = bd.authTokenScript.policyId + fromText(authTokenName)
    const mintValue = { [mp + fromText(adatag)]: mintAmount }
    const authToken = { [authUnit]: mintAmount }

    translucent.selectWalletFromSeed(deployerSeed)

    const isValid = isValidUsername(adatag)

    test(`Mint (${String(i++).padStart(3, ' ')}): should ${isValid ? '\x1b[32mPASS\x1b[0m' : '\x1b[31mFAIL\x1b[0m'}: "${adatag}"`, async () => {
      const tree = treeMap[authTokenName]
      const authUtxo = await translucent.utxoByUnit(authUnit)

      // Retrieve old state
      if (!authUtxo.datum) throw Error(`Invalid adatag state's datum in UTxO`)

      const oldState = Data.from(
        authUtxo.datum!,
        P.StateHolderStateHolder.oldState,
      )

      const result = tree.generateMinimalSubtree(adatag)
      const { updateVal, appendVal, proof } =
        result == null
          ? {
            appendVal: { xi: '0', xa: adatag, xb: adatag },
            updateVal: { xi: '0', xa: adatag, xb: adatag },
            proof: {
              NodeHash: { hash: '010101' },
            },
          }
          : result

      // append returns false when it cannot append
      if (isValid) {
        // we only allows valid adatags
        tree.append(adatag)
      }

      // TODO: we should have some optimised function from some proof instead getting the root hash form the whole tree
      const rootHash = tree.rootHash()

      // construct the new size
      const size = (
        parseInt(toText(oldState.size)) + Number(mintAmount)
      ).toString()

      // Construct the new state based on the old state's values
      const newState: P.StateHolderStateHolder['oldState'] = {
        operationCount: oldState.operationCount + mintAmount,
        adatag: fromText(adatag),
        operation: action as P.Operation,
        size: fromText(size),
        rootHash: rootHash,
        mintingPolicy: mp,
      }

      const state = Data.to(newState, P.StateHolderStateHolder.oldState)

      const rdmr = IntegriTree.buildRedeemer(updateVal, appendVal, proof)

      const timeStamp =
        translucent.provider instanceof Emulator
          ? translucent.provider.now()
          : Date.now()

      // zeros the millisecond parts of the validity range
      const from = Math.floor(timeStamp / 1000) * 1000
      const to = from + 60000

      const tx1 = translucent
        .newTx()
        .collectFrom([authUtxo], Data.void())

      // FIXME: implement timelock test
      const timelockActive = bd.adatagMinting.params.deactivationTime.epoch > from

      if (useTimelock && timelockActive) {
        // Times are in milliseconds in Plutus Core
        const deadLine = BigInt(
          to + bd.adatagMinting.params.lockingDays.ms + 10000
        )
        // IMPORTANT: to + bd.adatagMinting.params.lockingDays.ms < deadLine
        // console.warn(`Valid deadline: ${to + bd.adatagMinting.params.lockingDays.ms < deadLine}`);

        // Beneficiary is always the user!
        const { paymentCredential } = translucent.utils.getAddressDetails(userAddress!) as AddressDetails

        // Create the Timelock deposit datum
        const datum: P.TimeDepositDatum['datum'] = {
          beneficiary: paymentCredential!.hash as unknown as string,
          deadLine: deadLine,
        }

        const datumCbor = Data.to(datum, P.TimeDepositDatum.datum)

        // Retrieve the TielockDeposit's UTxO
        const timelockRefUtxo = await translucent!.utxosByOutRef([
          {
            txHash: bd.genesisTransaction,
            outputIndex: bd.timelockScript.refIndex,
          },
        ])
        refUtxos.concat(timelockRefUtxo)

        // TODO: Always check the contract's deposit calculation. It's in lovelace
        const deposit = calculateDeposit(adatag, 1750, 15, 6) * 1_000_000n

        tx1.payToContract(
          bd.timelockScript.scriptAddress,
          { inline: datumCbor },
          { lovelace: deposit },
        )
      }

      // If useTimelock is false, simply assign tx1 to ttx
      //const ttx = useTimelock ? tx2 : tx1; 

      const tx = tx1.payToAddress(userAddress, mintValue)
        .payToContract(
          bd.stateholderScript.scriptAddress,
          { inline: state },
          authToken,
        )
        .mintAssets(mintValue, rdmr)
        .readFrom(refUtxos)
        // This is required for deactivation's time checking.
        .validFrom(from)
        // This is required deadline checking
        .validTo(to)

      try {
        const completedTx = await tx.complete()
        const signedTx = await completedTx.sign().complete()
        const txHash = await signedTx.submit()
        
        expect(translucent.awaitTx(txHash)).resolves.toBe(true)
      } catch (e) {

        // If something throw an error then it must be triggered by an invalid adatag.
        // console.warn(`@@@@@@@@@@@ 1. ERROR: ${e}`)
        expect(isValid).toBe(false)
      }
    })
  }
})
