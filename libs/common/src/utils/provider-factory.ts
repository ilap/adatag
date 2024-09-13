import { Provider, Kupmios } from '@blaze-cardano/sdk'
import { Unwrapped } from '@blaze-cardano/ogmios'
import { Emulator, EmulatorProvider } from '@blaze-cardano/emulator'
import { TransactionOutput } from '@blaze-cardano/core'

import { Network } from './types'

import { ApiProvidersName } from '../config/index'
import apiUrls from '../config/api-urls.json'
import apiKeys from '../config/keys/api-keys.json'

export class ProviderFactory {
  private static isValidConfig(env: string, network: string, provider: string): boolean {
    // Define valid configurations for each combination of environment, network, and provider
    const validConfig: Record<string, Record<string, Record<string, unknown>>> = {
      Development: {
        Custom: {
          Emulator: {},
        },
      },
      Integration: {
        Custom: {
          Blockfrost: {},
          Maestro: {},
          Kupmios: {},
          KupmiosV5: {},
        },
        Preview: {
          Blockfrost: {},
          Maestro: {},
          Kupmios: {},
          KupmiosV5: {},
        },
      },
      Production: {
        Preprod: {
          Blockfrost: {},
          Maestro: {},
          Kupmios: {},
          KupmiosV5: {},
        },
        Mainnet: {
          Blockfrost: {},
          Maestro: {},
          Kupmios: {},
          KupmiosV5: {},
        },
      },
    }

    return validConfig[env]?.[network]?.[provider] !== undefined
  }

  static async createProvider(
    env: string,
    network: Network,
    provider: string,
    genesisAssets?: TransactionOutput[] | undefined
  ): Promise<Provider> {
    if (!ProviderFactory.isValidConfig(env, network, provider)) {
      throw new Error(`Invalid configuration for environment: ${env}, network: ${network}, provider: ${provider}`)
    }

    if ((provider !== 'Emulator' && genesisAssets) || (provider === 'Emulator' && !genesisAssets)) {
      throw new Error(`Unsupported configuration: ${provider} and emulator's genesis assets: ${genesisAssets}`)
    }

    // FIXME:
    const resolvedApikey =
      //Bun.env.API_KEY ?? undefined
      apiKeys?.[provider as ApiProvidersName]?.[network] ?? undefined

    switch (provider) {
      case 'Emulator':
        // eslint-disable-next-line
        return new EmulatorProvider(new Emulator(genesisAssets!))

      case 'Blockfrost':
        throw Error('Blockfrost: Unimplemented')
      case 'Maestro':
        throw Error('Maestro: Unimplemented')

      case 'Kupmios':
        const ogmios = await Unwrapped.Ogmios.new(apiUrls.Ogmios[network])
        return new Kupmios(apiUrls.Kupo[network], ogmios)
      case 'KupmiosV5':
        throw Error('KupmiosV5: Unimplemented')

      default:
        throw new Error(`Unsupported provider: ${provider}`)
    }
  }
}
