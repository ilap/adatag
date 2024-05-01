import { GenesisConfig } from '../utils/types'

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export async function getConfig(): Promise<GenesisConfig | undefined> {
  try {
    const response = await fetch('/genesis-config.json')
    const config = await response.json()
    return config
  } catch (e) {
    console.log(e)
    throw e
  }
}
