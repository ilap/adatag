import { describe, test, it, expect } from 'bun:test'
import { Address, Data, Emulator, Translucent, UTxO, fromText, toText, toUnit } from 'translucent-cardano'
import * as P from '@adatag/shared/plutus'
import { GenesisConfig, genesisParams } from '@adatag/shared/config'
import { Bootstrap } from '@adatag/shared/utils'
import { resolveMockData, setSloctConfig, stringifyData } from '@adatag/shared/test-utils'
import { emptyHash, hashVal, rootHash, IntegriTree } from '@adatag/integri-tree'
import { adatags } from './adatags'
/*
  https://github.com/input-output-hk/plutus-pioneer-program/blob/b55a7d2409cbf09a04ab471796f410083129acb6/code/Week03/lucid-ref-script/src/index.js

  Note: Topup the user's wallet
  Collector address : addr_test1qrqsm293uxd7zvs8yhaswenzzkkjxpfyfpaqufe0xjagp0hgyslwlf6ca9eend95lyw7pea32c2rtspq43sxd4a7sqwskerfjg
  Deployer address  : addr_test1qp0ueqgz64d3vns8j2tp4ef8jh9dgq5qevhdrg2tz3vw0fnzl28slr3x8ngs8x72w3jgsgeympuscxfyzl53yd4k0cas4u67dp
  Test User address : addr_test1qzza6achtargva760zfz8q37wfyl03sjgzev2rtsphew5r0qsh7mgst7chd99j6y6zqf00wx7whmydyjx2tqzxg0vv2qrn4s4m
*/
describe('Adatag minting', async () => {
  // The envs can be overwritten for dynamic testings, see and example below.
  //Bun.env.ENVIRONMENT = "Integration" //"Development" // "Integration";
  //Bun.env.NETWORK = "Custom";
  //Bun.env.PROVIDER = "KupmiosV5" //"Emulator" // "KupmiosV5";

  Bun.env.ENVIRONMENT = "Development" // "Integration";
  Bun.env.NETWORK = "Custom";
  Bun.env.PROVIDER = "Emulator" // "KupmiosV5";


  // eslint-disable-next-line @typescript-eslint/no-unused-vars
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

  // Access the params object for the specified network
  const params = genesisParams[network]

  interface Dictionary {
    [key: string]: IntegriTree;
  }

  // Create an empty dictionary object
  const treeMap: Dictionary = {};


  // Initialise the trees
  for (let i = 'a'.charCodeAt(0); i <= 'z'.charCodeAt(0); i++) {
    const char = String.fromCharCode(i);
    treeMap[char] = IntegriTree.fromLetter(char)
  }

  //const adatags = ["adam", "aranka", "attila", "alma", "alibaba", "alike", "alac", "abigel", "ada", "ad"]
  //["ilap", "gellert", "adam", "brenda", "loca", "agica", "samu", "csabi", "ben"]
  // const adatags = ["adi", "ar", "at", "al", "ali", "alik", "ala", "ab", "ada", "ad"]
  //const adatags = 

  //const adatags = sampleVals
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

  //deployed = true
  const bd: GenesisConfig = result
  // get the state holder reference
  const shRefUtxo = await translucent.utxosByOutRef([
    {
      txHash: bd.genesisTransaction,
      outputIndex: bd.stateholderScript.refIndex,
    },
  ])

  // get the minting policy reference
  const mpRefUtxo = await translucent.utxosByOutRef([
    {
      txHash: bd.genesisTransaction,
      outputIndex: bd.adatagMinting.refIndex,
    },
  ])

  const action: P.Operation = 'AdatagAdded'
  const mintAmount: bigint = 1n
  const mp = bd.adatagMinting.policyId

  for (const adatag of adatags.slice(0, 1000)) {

    if (translucent.provider instanceof Emulator) {
      translucent.provider.awaitBlock(10)
    }

    const authTokenName = adatag[0]
    const authUnit = bd.authTokenScript.policyId + fromText(authTokenName)
    const mintValue = { [mp + fromText(adatag)]: mintAmount }
    const authToken = { [authUnit]: mintAmount, }

    translucent.selectWalletFromSeed(deployerSeed)
    test(`On-chain mint adatag ${adatag}`, async () => {
      const tree = treeMap[authTokenName]
      const authUtxo = await translucent.utxoByUnit(authUnit)

      // Retrieve old state
      if (!authUtxo.datum) throw Error(`Invalid adatag state's datum in UTxO`)

      const oldState = Data.from(
        authUtxo.datum!,
        P.StateHolderStateHolder.oldState,
      )

      const oldRootHash = tree.rootHash()
      const { updateVal, appendVal, proof } = tree.generateMinimalSubtree(adatag)
      if (!tree.append(adatag)) {
        throw Error(`Cannot append adatag (${adatag}) into the tree`)
      }

      // TODO: we should have some optimised function from some proof instead getting the root hash for mthe whole tree
      const rootHash = tree.rootHash()

      const size = (parseInt(toText(oldState.size)) + Number(mintAmount)).toString()

      // Construct the new state based on the old state's values
      const newState: P.StateHolderStateHolder["oldState"] = {
        operationCount: oldState.operationCount + mintAmount,
        adatag: fromText(adatag),
        operation: action as P.Operation,
        size: fromText(size),
        rootHash: rootHash,
        mintingPolicy: mp,
      }

      const state = Data.to(newState, P.StateHolderStateHolder.oldState)

      const rdmr = IntegriTree.buildRedeemer(updateVal, appendVal, proof)

      const time =
        translucent.provider instanceof Emulator
          ? translucent.provider.now()
          : Date.now()


      const tx = await translucent
        .newTx()
        .collectFrom([authUtxo], Data.void())
        .readFrom(mpRefUtxo.concat(shRefUtxo))
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

  }

})
/*
  test(`On-chain mint adatag ${adatag}`, async () => {

    if (!tree.append(adatag)) {
      throw Error(`Cannot append adatag: "${adatag}"`)
    }

    //const { updateVal: uv1, appendVal: av1, proof: p1 }  = tree.generateMinimalSubtree(adatag)
    const rhu = tree.rootHash()



    console.log(`Hash of the old state: ${oldState.rootHash}`)
    console.log(`Hash of the new state: ${rhu}`)

    const newState: P.StateHolderStateHolder["oldState"] = {
      operationCount: oldState.operationCount + mintAmount,
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
    // const rootVal1 = { xi: fromText('0'), xa: fromText('`'), xb: fromText('b') }

    const mintRedeemer: P.MintRedeemer = {
      Minting: [
        {
          updateVal: updateVal,
          appendVal: appendVal,
          proof: proof,
        },
      ], // Use 'as const' to assert that Minting is a tuple with a single element
    }

    const rdmr = Data.to(mintRedeemer, P.AdatagAdatagMinting.rdmr, 'proof')
    console.log(`Redeemer: $${rdmr}`)

    translucent.selectWalletFromSeed(deployerSeed)
    const userAddress = await translucent.wallet.address()

    const time =
      translucent.provider instanceof Emulator
        ? translucent.provider.now()
        : Date.now()

    const tx = await translucent
      .newTx()
      .collectFrom([authUtxo], Data.void())
      .readFrom(mpRefUtxo.concat(shRefUtxo))
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
async function mintAdatag(adatag: string, translucent: Translucent, userAddress: Address, authUtxo: UTxO, shRefUtxo: UTxO[], mpRefUtxo: UTxO[], mintRedeemer: P.MintRedeemer, newState: NewState) {

  const rdmr = Data.to(mintRedeemer, P.AdatagAdatagMinting.rdmr, 'proof')
  console.log(`Redeemer: $${rdmr}`)

  const mintValue = { [mp + fromText(adatag)]: 1n }
  const authToken = {
    [bd.authTokenScript.policyId + fromText(adatag[0])]: 1n,
  }

  const time =
    translucent.provider instanceof Emulator
      ? translucent.provider.now()
      : Date.now()

  const tx = await translucent
    .newTx()
    .collectFrom([authUtxo], Data.void())
    .readFrom(mpRefUtxo.concat(shRefUtxo))
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


}
*/
