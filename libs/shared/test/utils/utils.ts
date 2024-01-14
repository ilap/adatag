import {
  Assets,
  MintingPolicy,
  Network,
  Translucent,
  generateSeedPhrase,
} from 'translucent-cardano'

export function assert(condition: unknown, msg?: string): asserts condition {
  if (condition === false) throw new Error(msg)
}

export function delayRandom(max: number) {
  const ms = Math.random() * max * 1000
  return new Promise(resolve => setTimeout(resolve, ms))
}

export async function generateAccountWithSeed(seed: string, assets: Assets) {
  return {
    seed,
    address: await (await Translucent.new(undefined, 'Custom'))
      .selectWalletFromSeed(seed)
      .wallet.address(),
    assets,
  }
}

export async function generateAccount(assets: Assets) {
  const seed = generateSeedPhrase()
  return generateAccountWithSeed(seed, assets)
}

export async function getPolicyId(mp: MintingPolicy): Promise<string> {
  return (await Translucent.new(undefined, 'Custom')).utils.mintingPolicyToId(
    mp,
  )
}

export function stringifyData(data: unknown) {
  return JSON.stringify(
    data,
    (key, value) => (typeof value === 'bigint' ? value.toString() : value),
    '  ',
  )
}
