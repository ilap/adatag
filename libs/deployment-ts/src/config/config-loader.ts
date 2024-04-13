import { GenesisConfig, GenesisParams } from './types'

export class ConfigLoader {
  private static readonly configPath = '../../configs'
  private static readonly activeNetwork: string =
  import.meta.env.NETWORK || 'Custom'

  public static async loadGenesisParams(): Promise<GenesisParams> {
    const filePath = `${this.configPath}/genesis-params-${this.activeNetwork.toLowerCase()}.json`
    
    console.log(`### GENESIS PARAMS: ${filePath}`)
    const response = await fetch(filePath);
    const jsonData = await response.json();
    return jsonData as GenesisParams;
  }

  public static async loadGenesisConfig(): Promise<GenesisConfig> {
    const filePath = `${this.configPath}/genesis-config-${this.activeNetwork.toLocaleLowerCase()}.json`
    
    console.log(`### GENESIS CONFIG: ${filePath}`)
    const response = await fetch(filePath);
    const jsonData = await response.json();
    return jsonData as GenesisConfig;
  }
}
