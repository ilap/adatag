import { test, expect } from 'bun:test'
import { Data, Translucent, fromText } from 'translucent-cardano'

import { resolveMockData, stringifyData } from '@adatag/common/utils'
import * as P from '@adatag/common/plutus'

test('Simple Auth NFT mint', async () => {

  const { deployerSeed, userSeed, network, provider } =
    await resolveMockData()

  // Always apply network to the constructor.
  const translucent = await Translucent.new(provider, network)

  // Select the receiving wallet.
  const userAddress = await translucent
    .selectWalletFromSeed(userSeed)
    .wallet.address()

  // Select the spending wallet.
  translucent.selectWalletFromSeed(deployerSeed)
  const [utxo] = await translucent.wallet.getUtxos()

  const authMintingPolicy = new P.OneshotAuthToken({
    transactionId: { hash: utxo.txHash },
    outputIndex: BigInt(utxo.outputIndex),
  })

  const policyId = translucent.utils.mintingPolicyToId(authMintingPolicy)

  const mintVal = { [policyId + fromText('a')]: 1n }
  const tx = await translucent
    .newTx()
    .collectFrom([utxo]) // with or without it
    // TODO: Translucent does not allow paying to different address than the signer.
    .payToAddress(userAddress, mintVal)
    .attachMintingPolicy(authMintingPolicy)
    .mintAssets(mintVal, Data.void())
    .complete()

  const signedTx = await tx.sign().complete()
  const txHash = await signedTx.submit()

  expect(translucent.awaitTx(txHash, 10000)).resolves.toBeDefined()
})
