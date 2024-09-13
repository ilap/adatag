import { test, expect } from 'bun:test'
import { Blaze, HotWallet } from '@blaze-cardano/sdk'
import { mintAdahandle, resolveMockData } from '@adatag/common/utils'

test('Minting mock-adahandle', async () => {
  const { deployerMasterkey, userMasterkey, network, provider } = await resolveMockData()

  const address = (await HotWallet.fromMasterkey(userMasterkey, provider)).address

  const wallet = await HotWallet.fromMasterkey(deployerMasterkey, provider)
  const blaze = await Blaze.from(provider, wallet)

  expect(mintAdahandle(blaze, ['ilap', 'pal', 'adam'], address)).resolves.toBeDefined()
})
