import * as apiUrls from '../../configs/api-urls.json'
import * as apiKeys from '../../keys/api-keys.json'
import * as genesisParams from '../../configs/genesis-params.json'

export type ApiProvidersName = "Blockfrost" | "Maestro";
export type ApiKeys = Record<ApiProvidersName, Record<string, string>>;
export type ApiUrls = Record<ApiProvidersName, Record<string, string>>;


export { ConfigLoader } from './config-loader'

// Deployed contracts related exports
export * from './types'

export {
    apiUrls,
    apiKeys,
    genesisParams
}