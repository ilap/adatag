import { GenesisConfig, GenesisParams } from './types'

export class ConfigLoader {
  private static readonly configPath = '../../configs'
  private static readonly activeNetwork: string =
    process.env.NETWORK || 'Preview'

  public static async loadGenesisParams(): Promise<GenesisParams> {
    const filePath = `${this.configPath}/genesis-params-${this.activeNetwork.toLowerCase()}.json`
    const module = await import(filePath)
    return module.default as GenesisParams
  }

  public static async loadGenesisConfig(): Promise<GenesisConfig> {
    const filePath = `${this.configPath}/genesis-config-${this.activeNetwork.toLocaleLowerCase()}.json`
    const module = await import(filePath)
    return module.default as GenesisConfig
  }
}

//console.log(await ConfigLoader.loadGenesisParams())
//console.log(await ConfigLoader.loadGenesisConfig())
