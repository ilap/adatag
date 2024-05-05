import { GenesisConfig } from '../utils/types'

export const genesisConfig: GenesisConfig | undefined = await getConfig()

// eslint-disable-next-line @typescript-eslint/no-explicit-any
async function getConfig(): Promise<GenesisConfig | undefined> {
  try {
    const response = await fetch('/genesis-config.json')
    return await response.json()
  } catch (e) {
    console.log(`Cannot fetch genesis-config: %{(e as Error)?.message}`)
    throw e
  }
}
