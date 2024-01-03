import {
  Assets,
  Blockfrost,
  Emulator,
  Kupmios,
  KupmiosV5,
  Maestro,
  MaestroSupportedNetworks,
  Network,
  OutputData,
} from "translucent-cardano";

import apiUrls from "../config/api-urls.json";
import apiKeys from "../keys/api-keys.json";

type ApiProvidersName = "Blockfrost" | "Maestro";

export class ProviderFactory {
  private static isValidConfig(
    env: string,
    network: string,
    provider: string,
  ): boolean {
    // Define valid configurations for each combination of environment, network, and provider
    const validConfig: Record<
      string,
      Record<string, Record<string, unknown>>
    > = {
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
          KupmiosV5: {},
        },
        Mainnet: {
          Blockfrost: {},
          Maestro: {},
          KupmiosV5: {},
        },
      },
    };

    return validConfig[env]?.[network]?.[provider] !== undefined;
  }

  static createProvider(
    env: string,
    network: Network,
    provider: string,
    genesisAssets?:
      | {
          address: string;
          assets: Assets;
          outputData?: OutputData | undefined;
        }[]
      | undefined,
  ): any {
    if (!ProviderFactory.isValidConfig(env, network, provider)) {
      throw new Error(
        `Invalid configuration for environment: ${env}, network: ${network}, provider: ${provider}`,
      );
    }

    if (
      (provider !== "Emulator" && genesisAssets) ||
      (provider === "Emulator" && !genesisAssets)
    ) {
      throw new Error(
        `Unsupported configuration: ${provider} and emulator's genesis assets: ${genesisAssets}`,
      );
    }

    const resolvedApikey =
      apiKeys?.[provider as ApiProvidersName]?.[network] ?? undefined;

    switch (provider) {
      case "Emulator":
        return new Emulator(genesisAssets!);

      case "Blockfrost":
        const url = apiUrls.Blockfrost[network];
        return new Blockfrost(url, resolvedApikey);

      case "Maestro":
        return new Maestro({
          // As it's a valid MaestroSupportedNetworks now, as it's passed the validation.
          network: network as MaestroSupportedNetworks,
          apiKey: resolvedApikey,
        });

      case "Kupmios":
        return new Kupmios(apiUrls.Kupo[network], apiUrls.Ogmios[network]);
      case "KupmiosV5":
        return new KupmiosV5(apiUrls.Kupo[network], apiUrls.Ogmios[network]);

      default:
        throw new Error(`Unsupported provider: ${provider}`);
    }
  }
}
