import { describe, test, expect } from 'bun:test'

import { Data, Emulator, Translucent, fromText } from 'translucent-cardano'

import * as P from '@adatag/shared/plutus'

import { GenesisConfig, genesisParams } from '@adatag/shared/config'
import { Bootstrap } from '@adatag/shared/utils'

import { resolveMockData, setSloctConfig } from '@adatag/shared/test-utils'

import { emptyHash, hashVal, rootHash } from '@adatag/integri-tree'

/*
  https://github.com/input-output-hk/plutus-pioneer-program/blob/b55a7d2409cbf09a04ab471796f410083129acb6/code/Week03/lucid-ref-script/src/index.js

  Note: Topup the user's wallet
  Collector address : addr_test1qrqsm293uxd7zvs8yhaswenzzkkjxpfyfpaqufe0xjagp0hgyslwlf6ca9eend95lyw7pea32c2rtspq43sxd4a7sqwskerfjg
  Deployer address  : addr_test1qp0ueqgz64d3vns8j2tp4ef8jh9dgq5qevhdrg2tz3vw0fnzl28slr3x8ngs8x72w3jgsgeympuscxfyzl53yd4k0cas4u67dp
  Test User address : addr_test1qzza6achtargva760zfz8q37wfyl03sjgzev2rtsphew5r0qsh7mgst7chd99j6y6zqf00wx7whmydyjx2tqzxg0vv2qrn4s4m
*/
describe('Adatag minting', async () => {
  // The envs can be overwritten for dynamic testings, see and example below.
  // Bun.env.ENVIRONMENT = "Integration";
  // Bun.env.NETWORK = "Custom";
  // Bun.env.PROVIDER = "KupmiosV5";
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const { deployerSeed, collectorSeed, userSeed, network, provider } =
    await resolveMockData()

  // If we want tu use validity ranges for transactin with private networks that use dynamic
  // startup time and slot length then we need to gather the proper parameters somehow.
  // - "Custom" assuming a private networ or Emulator.
  // - "Preview", "Preprod" and "Mainnet" assuming the well-know parameters.
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

    //deployed = true
    GenesisConfig = result
  })

  test('On-chain mint adatag', async () => {
    if (translucent.provider instanceof Emulator) {
      translucent.provider.awaitBlock(30)
    }

    const bd = GenesisConfig

    // The adatag is deployed, lets mint an @adatag
    const adatag = 'adam'

    // get the state holder reference
    const shref = await translucent.utxosByOutRef([
      {
        txHash: bd.genesisTransaction,
        outputIndex: bd.stateholderScript.refIndex,
      },
    ])

    // get the minting policy reference
    const mpref = await translucent.utxosByOutRef([
      {
        txHash: bd.genesisTransaction,
        outputIndex: bd.adatagMinting.refIndex,
      },
    ])

    // It's sittings at genesis transaction's 3rd index initially.
    // TODO: findAuthToken(bd.stateHolder.scriptAddress, bd.adatagMinting.policyId, token_name)
    const [auth_utxo] = await translucent.utxosByOutRef([
      { txHash: bd.genesisTransaction, outputIndex: 3 },
    ])

    // TODO: mkProof(tree, adatag) --> const (un, un1, an, proof, newTree) =
    const proof: P.Proof = oldProof
    const un1 = undefined // Simulating mint
    const { action, mintvalue } = un1
      ? { action: 'AdatagRemoved', mintvalue: -1n }
      : { action: 'AdatagAdded', mintvalue: 1n }

    const mp = bd.adatagMinting.policyId

    const rhu = rootHash(updatedState)

    // Retrieve old state
    if (!auth_utxo.datum) throw Error(`Invalid adatag state's datum in UTxO`)

    const oldState = Data.from(
      auth_utxo.datum!,
      P.StateHolderStateHolder.oldState,
    )

    const newState = {
      operationCount: oldState.operationCount + mintvalue,
      adatag: fromText(adatag),
      operation: action as P.Operation,
      size: fromText('1'), // Only added 1
      rootHash: rhu,
      mintingPolicy: mp,
    }

    const mintValue = { [mp + fromText(adatag)]: 1n }
    const authToken = {
      [bd.authTokenScript.policyId + fromText(adatag[0])]: 1n,
    }
    const state = Data.to(newState, P.StateHolderStateHolder.oldState)
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

    const time =
      translucent.provider instanceof Emulator
        ? translucent.provider.now()
        : Date.now()

    const tx = await translucent
      .newTx()
      .collectFrom([auth_utxo], Data.void())
      .readFrom(mpref.concat(shref))
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
      // It is always required for deactivation time checking.
      .validFrom(time)
      .complete()

    const signedTx = await tx.sign().complete()
    const txHash = await signedTx.submit()
    expect(translucent.awaitTx(txHash)).resolves.toBe(true)
  })
})
