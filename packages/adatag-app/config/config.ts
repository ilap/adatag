import {
    Blockfrost,
    Emulator,
    Kupmios,
    KupmiosV5,
    Maestro,
    Provider,
    MaestroSupportedNetworks,
    Translucent,
    Network,
    generateSeedPhrase,
    Assets,
    Address,
    OutputData,
} from "translucent-cardano";
import { mkdir, stat } from "node:fs/promises";

type AppParams = {
    bootstrapTime: number;
    depositBase: number;
    lockingDays: number;
    deactivationTime: number;
    collectionTime: number;
    adahandleSymbol: string;
};

async function handleIO(
    dir: string,
    file_name: string,
    generator: () => Promise<string> | string,
): Promise<string> {
    try {
        // Does not exist or no permission to read.
        await stat(dir);
    } catch {
        await mkdir(dir, { recursive: true });
    }
    const path = dir + file_name;
    try {
        return await Bun.file(path).text();
    } catch (_) {
        const data =
            typeof generator === "function" ? await generator() : generator;
        await Bun.write(path, data);
        return data;
    }
}

export class AppSettings {
    // Static settings for different users
    public static usersSettings = {
        keysDir: Bun.env.KEYS_DIR || "",
        ownerSeed: Bun.env.APP_OWNER_SEED || "",
        collectorSeed: Bun.env.COLLECTOR_SEED || "",
        userSeed: Bun.env.USER_SEED || "",
    };

    // Static settings related to the build
    public static buildSettings = {
        network: (Bun.env.NETWORK || "Preview") as Network,
        hashAlg: Bun.env.HASH_ALG || "",
        plutusVersion: Bun.env.PLUTUS_VERSION || "",
        environment: Bun.env.ENVIRONMENT || "",
    };

    // Static settings for providers
    public static providersSettings = {
        providerName: Bun.env.PROVIDER || "",
        maestroApiKey: Bun.env.MAESTRO_API_KEY || "",
        blockfrostUrl: Bun.env.BLOCKFROST_URL || "",
        blockfrostProjectId: Bun.env.BLOCKFROST_PROJECT_ID || "",
        ogmiosUrl: Bun.env.OGMIOS_URL || "",
        kupoUrl: Bun.env.KUPO_URL || "",
    };

    // Instance of the application
    private static instance: AppSettings;

    private constructor(
        public translucent: Translucent,
        public appParams: AppParams,
    ) { }

    public static days = (n: string) => Math.floor(eval(n) * 86400000);
    // Method to get a singleton instance of the AppSettings class
    public static async getInstance(
        accounts?: {
            address: Address;
            assets: Assets;
            outputData?: OutputData;
        }[],
    ) {
        if (!this.instance) {
            const network: Network = AppSettings.buildSettings.network;
            const provider = await AppSettings.setupProvider(
                AppSettings.providersSettings.providerName,
                network,
                accounts,
            );

            const translucent = await Translucent.new(provider, network);
            const appParams = await AppSettings.retrieveAppParams(
                AppSettings.providersSettings.providerName,
                provider,
            );

            this.instance = new AppSettings(translucent, appParams);
        }
        return this.instance;
    }

    // Method to retrieve application parameters
    public static async retrieveAppParams(
        providerName: string,
        provider: Provider,
    ): Promise<AppParams> {
        // Define the days lambda expression

        let bootstrapTime: number;

        if (provider && providerName === "Custom") {
            bootstrapTime = (provider as Emulator).now();
        } else {
            bootstrapTime = Date.now();
        }

        return {
            bootstrapTime: bootstrapTime,
            depositBase: parseInt(Bun.env.DEPOSIT_BASE || "") || 0,
            lockingDays: AppSettings.days(Bun.env.LOCKING_DAYS || ""),
            deactivationTime:
                bootstrapTime + AppSettings.days(Bun.env.DEACTIVATION_TIME || ""),
            collectionTime: bootstrapTime + AppSettings.days(Bun.env.COLLECTION_TIME || ""),
            adahandleSymbol: Bun.env.ADAHANDLE || "",
        };
    }

    public static async setupProvider(
        providerName: string,
        network: Network,
        accounts?: {
            address: Address;
            assets: Assets;
            outputData?: OutputData;
        }[],
    ) {
        let provider: Provider;

        switch (providerName) {
            case "Custom":
                provider = new Emulator(accounts || []);
                break;
            case "Blockfrost":
                provider = new Blockfrost(
                    AppSettings.providersSettings.blockfrostUrl,
                    AppSettings.providersSettings.blockfrostProjectId,
                );
                break;
            case "Maestro":
                if (network === "Custom") {
                    throw new Error(`Unsupported network`);
                }
                const nw: MaestroSupportedNetworks = AppSettings.buildSettings
                    .network as MaestroSupportedNetworks;
                provider = new Maestro({
                    network: nw,
                    apiKey: AppSettings.providersSettings.maestroApiKey,
                });
                break;
            case "KupmiosV5":
                provider = new KupmiosV5(
                    AppSettings.providersSettings.kupoUrl,
                    AppSettings.providersSettings.ogmiosUrl,
                );
                break;
            case "Kupmios":
                provider = new Kupmios(
                    AppSettings.providersSettings.kupoUrl,
                    AppSettings.providersSettings.ogmiosUrl,
                );
                break;
            default:
                throw new Error("Invalid provider or the .env is not sourced.");
        }

        return provider;
    }



    public showAppSettings() {
        console.log(
            "####################### dApp Parameters are ##########################",
        );
        console.log(`Network          : ${AppSettings.buildSettings.network}`);
        console.log(`Environment      : ${AppSettings.buildSettings.environment}`);
        console.log(
            "!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
        );
        console.log(`Hash algorithm   : ${AppSettings.buildSettings.hashAlg}`);
        console.log(`Plutus Version   : ${AppSettings.buildSettings.plutusVersion}`);
        console.log(
            "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
        );
        console.log(`Bootstrap time   : ${new Date(this.appParams.bootstrapTime)}`);
        console.log(
            `Deactivation time: ${new Date(this.appParams.deactivationTime)}`,
        );
        console.log(
            `Collection time  : ${new Date(this.appParams.collectionTime)}`,
        );

        console.log(
            `Locking days     : ${this.appParams.lockingDays / AppSettings.days("1")}`,
        );
        console.log(`Deposit base     : ${this.appParams.depositBase}`);
    }
}

//const i = await AppSettings.getInstance();
//i.showAppSettings();
