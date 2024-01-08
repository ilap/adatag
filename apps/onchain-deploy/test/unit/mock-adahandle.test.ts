import { test, expect } from 'bun:test'
import { Translucent } from 'translucent-cardano'
import { mintAdahandle } from '../utils/mock-adahandle.ts'
import { resolveMockData } from '../utils/resolve-mock-data.ts'

test('Minting mock-adahandle', async () => {
  // The envs can be overwritten for dynamic testings, see and example below.
  // Bun.env.ENVIRONMENT = "Development";
  // Bun.env.NETWORK = "Custom";
  // Bun.env.PROVIDER = "Emulator";

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const { deployerSeed, collectorSeed, userSeed, network, provider } =
    await resolveMockData()

  const translucent = await Translucent.new(provider, network)

  // Select the the receiving wallet.
  const address = await translucent
    .selectWalletFromSeed(userSeed)
    .wallet.address()

  translucent.selectWalletFromSeed(deployerSeed)

  expect(
    mintAdahandle(translucent, ['ilap', 'pali'], address),
  ).resolves.toBeDefined()
})
