import { Blaze, Data, Provider, Wallet } from '@blaze-cardano/sdk'
import * as P from '../../src/plutus/index.js'
import { AssetName, TokenMap, Value, Address, AssetId, type Script, PolicyId } from '@blaze-cardano/core'
import { cborToScript } from '@blaze-cardano/uplc'
import { fromText } from './utils'

export function getAdahandleScript(): Script {
  const adahandlePolicy = new P.AlwaysMintMint().script
  return cborToScript(adahandlePolicy, 'PlutusV2')
}

export function getAdahandlePolicyId() {
  const adahandlePolicy = getAdahandleScript()
  return adahandlePolicy.hash()
}

export async function mintAdahandle(blaze: Blaze<Provider, Wallet>, adatags: string[], toAddres: Address) {
  const adahandlePolicy = getAdahandleScript()
  const policyId = PolicyId(adahandlePolicy.hash())

  const mint: TokenMap = new Map()
  const assets: Map<AssetName, bigint> = new Map() //{}

  adatags.forEach(adatag => {
    mint.set(AssetId.fromParts(policyId, AssetName(fromText(adatag))), 1n)
    const assetName = AssetName(fromText(adatag))
    assets.set(assetName, 1n)
  })

  const value = new Value(0n, mint)

  const tx = await blaze
    .newTransaction()
    .payAssets(toAddres, value)
    .addMint(policyId, assets, Data.void())
    .provideScript(adahandlePolicy)
    .complete()

  const signed = await blaze.wallet.signTransaction(tx, true)
  const ws = tx.witnessSet()
  ws.setVkeys(signed.vkeys()!)
  tx.setWitnessSet(ws)
  const txId = await blaze.provider.postTransactionToChain(tx)
  const confirmed = await blaze.provider.awaitTransactionConfirmation(txId, 20_000)

  return confirmed ? txId : undefined
}
