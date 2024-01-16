import {
  Assets,
  MintingPolicy,
  Network,
  SLOT_CONFIG_NETWORK,
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

interface YaciInfo {
  nodePort: number
  submitApiPort: number
  socketPath: string
  protocolMagic: number
  slotLength: number
  blockTime: number
  epochLength: number
  p2pEnabled: boolean
  startTime: number
  masterNode: boolean
  adminNodeUrl: string | null
  ogmiosPort: number
  kupoPort: number
  yaciStorePort: number
  socatPort: number
  blockProducer: boolean
}
export async function setSloctConfig(network: Network, env: string) {
  // Only for
  if (!(network == 'Custom' && env == 'Integration')) {
    return
  }
  try {
    const response = await fetch(
      `http://localhost:10000/local-cluster/api/admin/clusters/default`,
    )

    if (response.ok) {
      const serverInfo: YaciInfo = await response.json()
      if (serverInfo.startTime !== 0) {
        SLOT_CONFIG_NETWORK[network] = {
          zeroTime: serverInfo.startTime * 1000,
          zeroSlot: 0,
          slotLength: serverInfo.slotLength * 1000,
        }
      }
    } else {
      throw Error(`Could not set the slot config`)
    }
  } catch (error) {
    throw Error(`Could not set the slot config`)
  }
}
