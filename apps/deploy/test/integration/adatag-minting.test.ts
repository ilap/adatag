import { describe, test, expect } from 'bun:test'
import * as P from '@adatag/common/plutus'
import { GenesisConfig, genesisParams } from '@adatag/common/config'
import { Bootstrap } from '../../src/lib/bootstrap'
import {
  setSlotConfig,
  isValidUsername,
  isLowerCase,
  calculateDeposit,
  stringifyData,
  resolveMockData,
  fromText,
  toText,
  unixTimeToSlot,
  generateRandomStrings,
} from '@adatag/common/utils'
import { IntegriTree } from '@adatag/integri-tree'

import { Blaze, Provider, HotWallet, Data } from '@blaze-cardano/sdk'

import {
  Address,
  TransactionInput,
  Value,
  AssetId,
  TokenMap,
  Ed25519KeyHashHex,
  PlutusData,
  HexBlob,
} from '@blaze-cardano/core'
import { EmulatorProvider } from '@blaze-cardano/emulator'

const delay = (ms: number) => new Promise(resolve => setTimeout(resolve, ms))

/*
  https://github.com/input-output-hk/plutus-pioneer-program/blob/b55a7d2409cbf09a04ab471796f410083129acb6/code/Week03/lucid-ref-script/src/index.js

  Note: Topup the user's wallet
  Collector address : addr_test1qzsk7aegh5rre3yhh5xl8r4k6vvkuqmf90fmfe9gkctu8tnpqamphkkru3r3p7va0yn0ws606fytvgq8gv4vaxekw3qs4r7hkk
  Deployer address  : addr_test1qrp6j8zuzqazju9x9kqksrmlqguypd6ku6xqu75m99zf76c2g9x9fz9yhe8n5h9k2x6uvws7s5aqqwdmkk3clt93tjcqc2ljnk
  Test User address : addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex
*/

// FIXME: only "a"'s for testin
const prefix = 'a' // or null

// It OOMs over 2K elements.
const mintNr = 1200

const denominator = prefix ? 10 : 100
// TODO:
const nr = Bun.env.ENVIRONMENT == 'Development' ? mintNr : Math.floor(mintNr / denominator)

// 16 is ok but we want to have some long name fails too.
const adatagLength = 18

// it could generate duplicates
const _adatags = generateRandomStrings(nr, adatagLength, prefix)
const adatags = ['a?12dss', 'awb_fygi', 'adam', 'ali', 'anima']

// real nr. of elements.
const elemsNumber = adatags.size // .length

describe(`Adatag minting (${elemsNumber})`, async () => {
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

  const collectorWallet = await HotWallet.fromMasterkey(collectorMasterkey, provider)
  const collectorAddress = collectorWallet.address

  const userWallet = await HotWallet.fromMasterkey(userMasterkey, provider)
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

  // Access the params object for the specified network
  const params = genesisParams[network]

  const testParams = {
    ...params,
    collectorAddress: collectorAddress.toBech32().toString(),
    collectionTime: useTimelock ? params.collectionTime : 0.0,
    deactivationTime: useTimelock ? params.deactivationTime : 0.0,
    lockingDays: useTimelock ? params.lockingDays : 0.0,
  }

  const deployerWallet = await HotWallet.fromMasterkey(deployerMasterkey, provider)
  const deployerBlaze = await Blaze.from(provider as Provider, deployerWallet)

  // Select spending wallet
  const [utxo] = await deployerWallet.getUnspentOutputs()

  const bd: GenesisConfig = await Bootstrap.deploy(deployerBlaze, utxo, testParams)

  network == 'Custom'
    ? provider instanceof EmulatorProvider
      ? await delay(1500)
      : await delay(25000)
    : await delay(60000)
  //console.log(`BD: ${stringifyData(bd)}`)
  // Set the minting- and state-hodler policies' reference UTxOs
  const refInputs = [
    new TransactionInput(bd.genesisTransaction, bd.stateholderScript.refIndex),
    new TransactionInput(bd.genesisTransaction, bd.adatagMinting.refIndex),
  ]

  const action: P.Operation = 'AdatagAdded'
  const mintAmount: bigint = 1n
  const mp = bd.adatagMinting.policyId

  let i = 0

  for (const adatag of adatags) {
    network == 'Custom'
      ? provider instanceof EmulatorProvider
        ? await delay(1500)
        : await delay(25000)
      : await delay(60000)

    const authTokenName = isLowerCase(adatag.charCodeAt(0))
      ? adatag[0]
      : String.fromCharCode(Math.floor(Math.random() * (122 - 97) + 97))

    const authAssetId = AssetId(bd.authTokenScript.policyId + fromText(authTokenName))
    const mpAssetId = AssetId(bd.adatagMinting.policyId + fromText(adatag))

    // Correctly create a TokenMap (Map<AssetId, bigint>)
    const authToken: TokenMap = new Map<AssetId, bigint>([[authAssetId, mintAmount]])
    const mintToken: TokenMap = new Map<AssetId, bigint>([[mpAssetId, mintAmount]])

    const isValid = isValidUsername(adatag)

    test(`Mint (${String(i++).padStart(3, ' ')}): should ${
      isValid ? '\x1b[32mPASS\x1b[0m' : '\x1b[31mFAIL\x1b[0m'
    }: "${adatag}"`, async () => {
      network == 'Custom'
        ? provider instanceof EmulatorProvider
          ? await delay(1500)
          : await delay(25000)
        : await delay(60000)

      const tree = treeMap[authTokenName]
      // FIXME: const authUtxo = await translucent.utxoByUnit(authUnit)
      const authUtxo = await provider.getUnspentOutputByNFT(authAssetId)
      // Retrieve old state

      if (authUtxo.output().datum() === undefined) throw Error(`Invalid adatag state's datum in UTxO`)

      const datum = authUtxo.output().datum()!.asInlineData()!
      const oldState = Data.from(datum, P.StateHolderStateHolder.oldState)

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
      const size = (parseInt(toText(oldState.size)) + Number(mintAmount)).toString()

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

      // zeros are the millisecond parts of the validity range
      const timeStamp = provider instanceof EmulatorProvider ? 1000 : Date.now()
      const from = Math.floor(timeStamp / 1000) * 1000 - (network === 'Preview' ? 600000 : 0)
      const to = from + (network === 'Preview' ? 3600000 : 180000)

      const tx1 = userBlaze.newTransaction().addInput(authUtxo, Data.void())

      // TODO: implement timelock test
      const timelockActive = bd.adatagMinting.params.deactivationTime.epoch > from
      // TODO: console.log(`Timelock active: ${timelockActive}`)
      if (useTimelock && timelockActive) {
        // Times are in milliseconds in Plutus Core
        const deadLine = BigInt(to + bd.adatagMinting.params.lockingDays.ms + 10000)
        // IMPORTANT: to + bd.adatagMinting.params.lockingDays.ms < deadLine
        // console.log(`Valid deadline: ${to + bd.adatagMinting.params.lockingDays.ms < deadLine}`);

        // Beneficiary is always the user!
        const paymentCredential = userAddress.asBase()?.getPaymentCredential()

        // Create the Timelock deposit datum
        const datum: P.TimeDepositDatum['datum'] = {
          beneficiary: paymentCredential!.hash as unknown as string,
          deadLine: deadLine,
        }

        const datumData = Data.to(datum, P.TimeDepositDatum.datum)

        // Retrieve the TimelockDeposit's UTxO
        const timelockRefInput = new TransactionInput(bd.genesisTransaction, bd.timelockScript.refIndex)

        refInputs.push(timelockRefInput)

        // TODO: Always check the contract's deposit calculation. It's in lovelace
        const deposit = calculateDeposit(adatag, 1750, 15, 6) * 1_000_000n

        tx1.lockLovelace(Address.fromBech32(bd.timelockScript.scriptAddress), deposit, datumData)
      }

      const mintPolid = AssetId.getPolicyId(mpAssetId)
      const mintAsset = AssetId.getAssetName(mpAssetId)

      const refUtxos = await provider.resolveUnspentOutputs(refInputs)

      let tx2 = tx1
        .addMint(mintPolid, new Map([[mintAsset, 1n]]), rdmr)
        .addReferenceInput(refUtxos[0])
        .addReferenceInput(refUtxos[1])

      if (refUtxos.length > 2) {
        tx2 = tx2.addReferenceInput(refUtxos[2])
      }

      const fromSlot = unixTimeToSlot(from, network)
      const toSlot = unixTimeToSlot(to, network)

      const hash = userAddress.asBase()!.getPaymentCredential().hash.toString()
      const signerHash = Ed25519KeyHashHex(hash)

      const inputs = await userWallet.getUnspentOutputs()

      const tx = tx2
        .payAssets(userAddress, new Value(0n, mintToken), Data.void())
        .addUnspentOutputs(inputs)
        .lockAssets(Address.fromBech32(bd.stateholderScript.scriptAddress), new Value(0n, authToken), state)
        .setValidFrom(fromSlot)
        .setValidUntil(toSlot)

      try {
        const completedTx = await tx.complete()

        const signed = await userWallet.signTransaction(completedTx, true)
        const ws = completedTx.witnessSet()
        ws.setVkeys(signed.vkeys()!)
        completedTx.setWitnessSet(ws)

        const txHash = await provider.postTransactionToChain(completedTx)
        const isConfirmed = await provider.awaitTransactionConfirmation(txHash)

        expect(isConfirmed).toBe(true)
        expect(txHash).toBeTruthy()
      } catch (e) {
        // If something throw an error then it must be triggered by an invalid adatag.
        console.log(`Error during minting, see details: ${e}`)
        expect(isValid).toBe(false)
      }
    })
  }
})
