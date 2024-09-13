import { describe, test, expect } from 'bun:test'
import { Bootstrap } from '../../src/lib/bootstrap'
import { GenesisConfig, genesisParams } from '@adatag/common/config'
import { Blaze, Provider, Wallet, HotWallet, Data } from '@blaze-cardano/sdk'
import { Address, Bip32PrivateKeyHex, Ed25519KeyHashHex, Slot, TransactionInput, Value } from '@blaze-cardano/core'
import {
  resolveMockData,
  stringifyData,
  setSlotConfig,
  unixTimeToSlot,
  SLOT_CONFIG_NETWORK,
} from '@adatag/common/utils'
import { EmulatorProvider } from '@blaze-cardano/emulator'
import * as P from '@adatag/common/plutus'

const delay = (ms: number) => new Promise(resolve => setTimeout(resolve, ms))

describe('Timelock Deposit Tests', async () => {
  const { deployerMasterkey, userMasterkey, collectorMasterkey, network, provider } = await resolveMockData()

  // If we want to use validity ranges for transactin with private networks that use dynamic
  // startup time and slot length then we need to gather the proper parameters somehow.
  // - "Custom" assuming a private networ or Emulator.
  // - "Preview", "Preprod" and "Mainnet" assuming the well-know parameters.
  setSlotConfig(network, Bun.env.ENVIRONMENT || '')
  console.log(`SLOT_CONFIG_NETWORK: ${JSON.stringify(SLOT_CONFIG_NETWORK[network])}`)

  const collectorWallet = await HotWallet.fromMasterkey(collectorMasterkey!, provider)
  const collectorBlaze: Blaze<Provider, Wallet> = await Blaze.from(provider, collectorWallet)

  // Select the receiving wallet.
  const collectorAddress = (await HotWallet.fromMasterkey(collectorMasterkey, provider)).address

  // Access the params object for the specified network
  const params = await genesisParams[network]

  let genesisConfig: GenesisConfig

  test('On-chain deployment', async () => {
    // Create a newParams object by spreading the values from the original params
    const testParams = {
      ...params,
      collectorAddress: collectorAddress.toBech32().toString(),
      collectionTime: 0,
      deactivationTime: 0,
      lockingDays: 0,
    }

    console.log(`testParams: ${stringifyData(testParams)}`)

    // Select spending wallet
    const deployerWallet = await HotWallet.fromMasterkey(deployerMasterkey!, provider)
    const [utxo] = await deployerWallet.getUnspentOutputs()

    console.log(`utxos: ${stringifyData(utxo.toCore())}`)

    const deployerBlaze: Blaze<Provider, Wallet> = await Blaze.from(provider, deployerWallet)
    const result = await Bootstrap.deploy(deployerBlaze, utxo, testParams)
    expect(result).toBeDefined()

    genesisConfig = result
    delay(2500)
  })

  test('Deposit timelock', async () => {
    await delay(2500)

    const userWallet = await HotWallet.fromMasterkey(userMasterkey!, provider)
    const userBlaze = await Blaze.from(provider, userWallet)

    const tx = await userBlaze
      .newTransaction()
      .lockLovelace(Address.fromBech32(genesisConfig.timelockScript.scriptAddress), 50_000_000n, Data.void())
      .complete()

    const signed = await userBlaze.wallet.signTransaction(tx, true)
    const ws = tx.witnessSet()
    ws.setVkeys(signed.vkeys()!)
    tx.setWitnessSet(ws)

    const txHash = await userBlaze.provider.postTransactionToChain(tx)

    expect(userBlaze.provider.awaitTransactionConfirmation(txHash)).resolves.toBe(true)
    expect(txHash).toBeTruthy()
  })

  test('Collect with all time passed', async () => {
    await delay(2500)

    const lovelace = 1_500_000_000n

    const [spendingUtxo] = await provider.getUnspentOutputs(
      Address.fromBech32(genesisConfig.timelockScript.scriptAddress)
    )
    const timelockRedeemer = 'Collect'

    const refInput = new TransactionInput(genesisConfig.genesisTransaction, genesisConfig.timelockScript.refIndex)
    const [refUtxo] = await provider.resolveUnspentOutputs([refInput])
    const rdmr = Data.to(timelockRedeemer, P.TimeDepositTimedeposit.rdmr)

    // Set the timelock to the current time or some value for emulator for having a non-zero slot.
    const now = provider instanceof EmulatorProvider ? 10_000 : Date.now()

    const slot = unixTimeToSlot(now, network)

    const hash = collectorAddress.asBase()!.getPaymentCredential().hash.toString()
    const signerHash = Ed25519KeyHashHex(hash)

    const tx = await collectorBlaze
      .newTransaction()
      .addInput(spendingUtxo, rdmr)
      .addReferenceInput(refUtxo)
      .addRequiredSigner(signerHash)
      .payLovelace(collectorAddress, lovelace)
      .setValidFrom(slot)
      .complete()

    const signed = await collectorWallet.signTransaction(tx, true)
    const ws = tx.witnessSet()
    ws.setVkeys(signed.vkeys()!)
    tx.setWitnessSet(ws)

    const txHash = await provider.postTransactionToChain(tx)

    expect(provider.awaitTransactionConfirmation(txHash)).resolves.toBe(true)
    expect(txHash).toBeTruthy()
  })
})
