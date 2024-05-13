// - genesis-config-${network}.json // This is a generated config by deploy.
// - genesis-params.json // Static params, that contains all networks' params.
export * from './types'

import * as apiUrls from './api-urls.json'
import * as apiKeys from './keys/api-keys.json'
import * as genesisParams from './genesis-params.json'

export type ApiProvidersName = 'Blockfrost' | 'Maestro'
export type ApiKeys = Record<ApiProvidersName, Record<string, string>>
export type ApiUrls = Record<ApiProvidersName, Record<string, string>>

export { apiUrls, apiKeys, genesisParams }
