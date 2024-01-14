import { describe, test, expect } from 'bun:test'
import { SLOT_CONFIG_NETWORK } from 'translucent-cardano'

import * as P from '@adatag/shared/plutus'

import { Assets, Data, Emulator, Translucent } from 'translucent-cardano'

import { Bootstrap } from '@adatag/shared/utils'
import { GenesisConfig, genesisParams } from '@adatag/shared/config'
import { resolveMockData } from '@adatag/shared/test-utils'

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
      zeroSlot: 1704636741 * 1000,
      slotLength: 1000,
    }
  }
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

    GenesisConfig = result
  })

  /***********************************************************************************************
   *
   *
   ***********************************************************************************************/
  test('Deposit timelock', async () => {
    if (translucent.provider instanceof Emulator) {
      translucent.provider.awaitBlock(30)
    }

    const bd = GenesisConfig

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
    test('Collect with all time passed', async () => {
      if (translucent.provider instanceof Emulator) {
        translucent.provider.awaitBlock(30)
      }

      const bd = GenesisConfig

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

      //const signedTx =
      await tx.sign().complete()
      //const txHash = await signedTx.submit()
      //expect(translucent.awaitTx(txHash)).resolves.toBe(true)
    })
})
