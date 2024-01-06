import { describe, test, expect, afterEach, beforeEach, jest } from 'bun:test'
import * as P from '../../common/plutus.ts'

import {
  Assets,
  Data,
  Datum,
  Emulator,
  OutRef,
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
import { SLOT_CONFIG_NETWORK } from 'translucent-cardano'

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
  // Bun.env.ZERO_TIME=

  const { deployerSeed, collectorSeed, userSeed, network, provider } =
    await resolveMockData()

  const translucent = await Translucent.new(provider, network)

  // If we want tu use validity ranges for transactin with private networks that use dynamic 
  // startup time and slot length then we need to gather the proper parameters somehow.
  // - "Custom" assuming a private networ or Emulator.
  // - "Preview", "Preprod" and "Mainnet" assuming the well-know parameters.
  if (!(provider instanceof Emulator) && network === 'Custom') {
    SLOT_CONFIG_NETWORK[translucent.network] = {
      zeroTime: Date.now(),
      zeroSlot: 1704573394 * 1000,
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

  /***********************************************************************************************
   *
   *
   ***********************************************************************************************/
  test('Deposit timelock', async () => {
    if (translucent.provider instanceof Emulator) {
      translucent.provider.awaitBlock(30)
    }

    const bd = bootstrapDetails

    const lovelace: Assets = { lovelace: 1_500_000_000n }

    translucent.selectWalletFromSeed(userSeed)
    const tx = await translucent
      .newTx()
      .payToContract(
        bd.timelockScript.scriptAddress,
        { inline: Data.void() },
        lovelace,
      )
      .complete()

    const signedTx = await tx.sign().complete()
    const txHash = await signedTx.submit()
    expect(translucent.awaitTx(txHash)).resolves.toBe(true)
  }),
    test('On-chain mint adatag', async () => {
      if (translucent.provider instanceof Emulator) {
        translucent.provider.awaitBlock(30)
      }

      const bd = bootstrapDetails

      const lovelace: Assets = { lovelace: 1_500_000_000n }

      const utxoRef = await translucent.utxosByOutRef([
        {
          txHash: bd.genesisTransaction,
          outputIndex: bd.timelockScript.refIndex,
        },
      ])

      const [spendingUtxo] = await translucent.utxosAt(
        bd.timelockScript.scriptAddress,
      )

      const timelockRedeemer = 'Collect'

      const rdmr = Data.to(timelockRedeemer, P.TimeDepositTimedeposit.rdmr)

      translucent.selectWalletFromSeed(collectorSeed)
      const userAddress = await translucent.wallet.address()

      const now = Date.now()

      const tx = await translucent
        .newTx()
        .readFrom(utxoRef)
        .collectFrom([spendingUtxo], rdmr)
        .addSigner(collectorAddress)
        .payToAddress(userAddress, lovelace)
        .validFrom(now)
        .complete()

      const signedTx = await tx.sign().complete()
      //const txHash = await signedTx.submit()
      //expect(translucent.awaitTx(txHash)).resolves.toBe(true)
    })
})
