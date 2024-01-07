import { describe, test, expect, afterEach, beforeEach, jest } from 'bun:test'
import * as P from '../../common/plutus.ts'

import {
  Data,
  Datum,
  Emulator,
  OutRef,
  SLOT_CONFIG_NETWORK,
  Translucent,
  fromText,
} from 'translucent-cardano'
import * as PlutusParams from '../../config/plutus-params.json'
import { Bootstrap } from '../../deploy/bootstrap.ts'
import { BootstrapDetails } from '../../deploy/types.ts'

import { resolveMockData } from '../utils/resolve-mock-data.ts'
import { StateHolderStateHolder } from '../../common/plutus.ts'
import {
  Val,
  emptyHash,
  hashVal,
  rootHash,
} from '../../lib/labeled-tree/types.ts'

type Operation = 'AdatagAdded' | 'AdatagRemoved'

const stringifyData = (data: any) =>
  JSON.stringify(
    data,
    (key, value) => (typeof value === 'bigint' ? value.toString() : value),
    '  ',
  )

describe('Adatag minting', async () => {
  // The envs can be overwritten for dynamic testings, see and example below.
  // Bun.env.ENVIRONMENT = "Integration";
  // Bun.env.NETWORK = "Custom";
  // Bun.env.PROVIDER = "KupmiosV5";
  const { deployerSeed, collectorSeed, userSeed, network, provider } =
    await resolveMockData()

  const translucent = await Translucent.new(provider, network)

  if (!(provider instanceof Emulator) && network === 'Custom') {
    SLOT_CONFIG_NETWORK[translucent.network] = {
      zeroTime: Date.now(),
      zeroSlot: 1704636741 * 1000,
      slotLength: 1000,
    }
  }
  // Select the receiving wallet.
  const collectorAddress = await translucent
    .selectWalletFromSeed(collectorSeed)
    .wallet.address()

  // Access the params object for the specified network
  const params = PlutusParams[network]

  let deployed = false
  let bootstrapDetails: BootstrapDetails

  // Proof details
  // Initial tree of Ta
  const rootVal = { xi: '0', xa: '`', xb: 'b' }
  const updatedRootVal = { xi: '0', xa: '`', xb: 'adam' }
  const nodeVal = { xi: '1', xa: 'adam', xb: 'b' }

  const e: P.Proof = { NodeHash: { hash: emptyHash } }

  const oldProof: P.Proof = {
    HashNode: {
      // (0, 'ilap', 'b')
      hash: hashVal(rootVal),
      left: e,
      right: e,
    },
  }

  // from (0, '`', 'b') e, e
  // to ((0, '`', 'ilap'), ( (1, 'ilap', 'b'), e, e), e)
  const updatedState: P.Proof = {
    HashNode: {
      // (0, '`', 'ilap')
      hash: hashVal(updatedRootVal),
      left: {
        HashNode: {
          // (1, 'ilap', 'b')
          hash: hashVal(nodeVal),
          left: e,
          right: e,
        },
      },
      right: e,
    },
  }

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

    deployed = true
    bootstrapDetails = result
  })

  test('On-chain mint adatag', async () => {
    if (translucent.provider instanceof Emulator) {
      translucent.provider.awaitBlock(30)
    }

    const bd = bootstrapDetails

    // The adatag is deployed, lets mint an @adatag
    const adatag = 'adam'

    //console.log(`Stateholder Hash: ${bd.stateholderScript.scriptHash}`)
    //console.log(`Minting plolicy: ${bd.adatagMinting.policyId}`)

    const shref = await translucent.utxosByOutRef([
      {
        txHash: bd.genesisTransaction,
        outputIndex: bd.stateholderScript.refIndex,
      },
    ])

    //console.log(`Stakeholder Ref UTxO: ${stringifyData(shref)}`)

    const mpref = await translucent.utxosByOutRef([
      {
        txHash: bd.genesisTransaction,
        outputIndex: bd.adatagMinting.refIndex,
      },
    ])

    // console.log(`Stakeholder Ref UTxO: ${stringifyData(mpref)}`)

    if (params.deactivationTime > Date.now()) {
      const deposit = 1750 // TODO: calcDeposit(adatag, deploy.adatag_minting.params.deposit_base)
      const deadline =
        Date.now() + Bootstrap.days(params.lockingDays.toString())
      const adahandleUtxo = undefined // TODO: findWalletUtxosWithValue(translucent, params.adahandle, adatag)
    }

    // It's sittings at genesis transaction's 3rd index initially.
    // TODO: findAuthToken(bd.stateHolder.scriptAddress, bd.adatagMinting.policyId, token_name)
    const [auth_utxo] = await translucent.utxosByOutRef([
      { txHash: bd.genesisTransaction, outputIndex: 3 },
    ])

    // TODO: mkProof(tree, adatag) --> const (un, un1, an, proof, newTree) =
    // Build new proof
    // const un = rootVal
    // const an = rootVal
    const proof: P.Proof = oldProof
    const un1 = undefined // Simulating mint
    const { action, mintvalue } = un1
      ? { action: 'AdatagRemoved', mintvalue: -1n }
      : { action: 'AdatagAdded', mintvalue: 1n }

    const mp = bd.adatagMinting.policyId

    const rh = rootHash(oldProof)
    const rhu = rootHash(updatedState)

    // Retrieve old state
    if (!auth_utxo.datum) throw Error(`Invalid adatag state's datum in UTxO`)

    const oldState = Data.from(
      auth_utxo.datum!,
      P.StateHolderStateHolder.oldState,
    )

    //console.log(`Old state: ${stringifyData(oldState)}`)

    const newState = {
      operationCount: oldState.operationCount + mintvalue,
      adatag: fromText(adatag),
      operation: action as Operation,
      size: fromText('1'), // Only added 1
      rootHash: rhu,
      mintingPolicy: mp,
    }

    // Construct mintingpolicy redeem

    //console.log(`New state: ${stringifyData(newState)}`)

    const mintValue = { [mp + fromText(adatag)]: 1n }
    const authToken = {
      [bd.authTokenScript.policyId + fromText(adatag[0])]: 1n,
    }
    const state = Data.to(newState, P.StateHolderStateHolder.oldState)
    //console.log(`### NEW_STATE: ${state}`)
    const rootVal1 = { xi: fromText('0'), xa: fromText('`'), xb: fromText('b') }
    const mintRedeemer: P.MintRedeemer = {
      Minting: [
        {
          updateVal: rootVal1,
          appendVal: rootVal1,
          proof: proof,
        },
      ], // Use 'as const' to assert that Minting is a tuple with a single element
    }

    const rdmr = Data.to(mintRedeemer, P.AdatagAdatagMinting.rdmr, 'proof')

    translucent.selectWalletFromSeed(deployerSeed)
    const userAddress = await translucent.wallet.address()
    const [user_utxo] = await translucent.wallet.getUtxos()

    //console.log(`Redeemer : ${rdmr}`)
    //console.log(`User money: ${stringifyData(user_utxo.assets)}`)
    //console.log(`Autr money: ${stringifyData(auth_utxo.assets)}`)

    //console.log(`SH UTXO: ${stringifyData(shref)}`)
    //console.log(`SH Addr   : ${bd.stateholderScript.scriptAddress}`)
    //console.log(`SH Hash   : ${bd.stateholderScript.scriptHash}`)
    //console.log(`MP Plolicy: ${bd.adatagMinting.policyId}`)

    const time =
      translucent.provider instanceof Emulator
        ? translucent.provider.now()
        : Date.now()

    const [aaa_utxo] = await translucent.utxosByOutRef([
      { txHash: bd.genesisTransaction, outputIndex: 2 },
    ])
    //console.log(`SH Hash: ${bd.stateholderScript.scriptHash}`)
    //console.log(`SH Addr: ${bd.stateholderScript.scriptAddress}`)

    const tx = await translucent
      .newTx()
      .collectFrom([auth_utxo], Data.void())
      .readFrom(mpref.concat(shref))
      //.collectFrom(shref, Data.void())
      //.readFrom(shref)
      //.readFrom(shref)
      //.collectFrom([user_utxo])
      // Optional when deactivation time (16) is not reached
      // .paytoContract(timelockdeposit, { deposit }, { benef, now+ deadline}, noscript) // (8, 15,18) Optional, not required after deactivation time.
      // .collectFrom([adahandle_utxo]) // (17)
      .payToAddress(userAddress, mintValue)
      .payToContract(
        bd.stateholderScript.scriptAddress,
        { inline: state },
        authToken,
      )
      .mintAssets(mintValue, rdmr)
      // It is always required, as for deactivation time checking.
      .validFrom(time)
      .complete()

    const signedTx = await tx.sign().complete()
    //const txHash = await signedTx.submit()
    //expect(translucent.awaitTx(txHash)).resolves.toBe(true)
  })
})
