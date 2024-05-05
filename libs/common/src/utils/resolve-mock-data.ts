import { Network, Provider } from 'translucent-cardano'
import { ProviderFactory } from './provider-factory'
import { generateAccountWithSeed } from './utils'
import * as Seeds from '../config/keys/test-users-seed.json'

export function resolveEnvs(): {
  envStr: string
  networkStr: string
  providerStr: string
} {
  const envStr = Bun.env.ENVIRONMENT || ''
  const networkStr = Bun.env.NETWORK || ''
  const providerStr = Bun.env.PROVIDER || ''
  if (envStr === '' || networkStr === '' || providerStr === '')
    throw Error(
      `The required env variables not set: ENVIRONMENT ${envStr}, NETWORK: ${networkStr}, PROVIDER ${providerStr}.`
    )

  return { envStr, networkStr, providerStr }
}

export async function resolveMockData(): Promise<{
  deployerSeed: string
  collectorSeed: string
  userSeed: string
  network: Network
  provider: Provider
}> {
  const { envStr, networkStr, providerStr } = resolveEnvs()
  const deployerSeed = Seeds.deployer.seed
  const collectorSeed = Seeds.collector.seed
  const userSeed = Seeds.user.seed

  let provider: Provider

  const network: Network = networkStr as Network

  if (providerStr === 'Emulator') {
    const deployer = await generateAccountWithSeed(deployerSeed, {
      lovelace: 10_000_000_000_000n,
    })
    const collector = await generateAccountWithSeed(collectorSeed, {
      lovelace: 10_000_000_000_000n,
    })
    const user = await generateAccountWithSeed(userSeed, {
      lovelace: 10_000_000_000_000n,
    })
    provider = ProviderFactory.createProvider(envStr, network, providerStr, [deployer, collector, user])
  } else {
    provider = ProviderFactory.createProvider(envStr, network as Network, providerStr)
  }

  return { deployerSeed, collectorSeed, userSeed, network, provider }
}
