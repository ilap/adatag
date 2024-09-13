import { test, expect } from 'bun:test'
import { Blaze, Data, HotWallet } from '@blaze-cardano/sdk'
import { AssetName, TokenMap, Value, AssetId, PolicyId } from '@blaze-cardano/core'
import { resolveMockData, fromText } from '@adatag/common/utils'
import { cborToScript } from '@blaze-cardano/uplc'

import * as P from '@adatag/common/plutus'

test('Simple Auth NFT mint', async () => {
  const { deployerMasterkey, userMasterkey, network, provider } = await resolveMockData()

  // Always apply network to the constructor.
  const wallet = await HotWallet.fromMasterkey(deployerMasterkey, provider)
  const blaze = await Blaze.from(provider, wallet)

  // Select the receiving wallet.
  const userAddress = (await HotWallet.fromMasterkey(userMasterkey, provider)).address

  // Select the spending wallet.
  const [utxo] = await wallet.getUnspentOutputs()

  const txId = utxo.input().transactionId().toString()
  const idx = utxo.input().index()

  const authMintingPolicyScript = new P.OneshotAuthToken({
    transactionId: { hash: txId },
    outputIndex: idx,
  }).script

  const authMintingPolicy = cborToScript(authMintingPolicyScript, 'PlutusV2')

  const policyId = PolicyId(authMintingPolicy.hash())

  const mint: TokenMap = new Map()
  const assetName = AssetName(fromText('a'))
  const asset: Map<AssetName, bigint> = new Map()
  asset.set(assetName, 1n)

  mint.set(AssetId.fromParts(policyId, assetName), 1n)

  const value = new Value(0n, mint)

  const tx = await blaze
    .newTransaction()
    .payAssets(userAddress, value)
    .addMint(policyId, asset, Data.void())
    .provideScript(authMintingPolicy)
    .complete()

  const signed = await blaze.wallet.signTransaction(tx, true)
  const ws = tx.witnessSet()
  ws.setVkeys(signed.vkeys()!)
  tx.setWitnessSet(ws)

  console.log(`Posting tx: ${tx.toCbor()}`)
  expect(blaze.provider.postTransactionToChain(tx)).resolves.toBeDefined()
})
