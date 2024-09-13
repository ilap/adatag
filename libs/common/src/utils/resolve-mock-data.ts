import { wordlist, mnemonicToEntropy, Bip32PrivateKey } from '@blaze-cardano/core'
import { ProviderFactory } from './provider-factory'
import { generateAccountWithSeed } from './utils'
import * as Seeds from '../config/keys/test-users-seed.json'
import { Provider } from '@blaze-cardano/sdk'
import { Bip32PrivateKeyHex } from '@blaze-cardano/core'
import { Network } from './types'

function getEnvVar(name: string): string {
  const value = Bun.env[name] || ''
  if (value === '') throw new Error(`The required env variable ${name} is not set.`)
  return value
}

export async function resolveMockData(): Promise<{
  deployerMasterkey?: Bip32PrivateKeyHex
  collectorMasterkey?: Bip32PrivateKeyHex
  userMasterkey?: Bip32PrivateKeyHex
  network: Network
  provider: Provider
}> {
  const envStr = getEnvVar('ENVIRONMENT')
  const networkStr = getEnvVar('NETWORK')
  const providerStr = getEnvVar('PROVIDER')

  const network: Network = networkStr as Network

  if (providerStr === 'Emulator') {
    const generateAccount = (seed: string) => generateAccountWithSeed({ seed, amount: 100000000n })
    const { utxo: deployer, masterkeyHex: deployerMasterkey } = await generateAccount(Seeds.deployer.seed)
    const { utxo: collector, masterkeyHex: collectorMasterkey } = await generateAccount(Seeds.collector.seed)
    const { utxo: user, masterkeyHex: userMasterkey } = await generateAccount(Seeds.user.seed)
    const provider = await ProviderFactory.createProvider(envStr, network, providerStr, [
      ...deployer,
      ...collector,
      ...user,
    ])
    return { deployerMasterkey, collectorMasterkey, userMasterkey, network, provider }
  } else {
    const masterkeyFromMnenomic = (mnemonic: string) =>
      Bip32PrivateKey.fromBip39Entropy(Buffer.from(mnemonicToEntropy(mnemonic, wordlist)), '').hex()
    const deployerMasterkey = masterkeyFromMnenomic(Seeds.deployer.seed)
    const collectorMasterkey = masterkeyFromMnenomic(Seeds.collector.seed)
    const userMasterkey = masterkeyFromMnenomic(Seeds.user.seed)
    const provider = await ProviderFactory.createProvider(envStr, network, providerStr)
    return { deployerMasterkey, collectorMasterkey, userMasterkey, network, provider }
  }
}
