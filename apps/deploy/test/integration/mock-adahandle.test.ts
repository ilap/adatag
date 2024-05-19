import { test, expect } from 'bun:test'
import { Translucent } from 'translucent-cardano'
import { mintAdahandle, resolveMockData } from '@adatag/common/utils'

test('Minting mock-adahandle', async () => {
  const { deployerSeed, userSeed, network, provider } = await resolveMockData()

  const translucent = await Translucent.new(provider, network)

  // Select the the receiving wallet.
  const address = await translucent.selectWalletFromSeed(userSeed).wallet.address()

  translucent.selectWalletFromSeed(deployerSeed)

  expect(mintAdahandle(translucent, ['ilap', 'pal'], address)).resolves.toBeDefined()
})
