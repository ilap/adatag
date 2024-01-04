import { test, expect, describe } from "bun:test";
import { Translucent } from "translucent-cardano";
import * as PlutusParams from "../../config/plutus-params.json";
import { Bootstrap } from "../../deploy/bootstrap.ts";
import { addressFromSeed } from "../utils/utils.ts";
import { resolveMockData } from "../utils/resolve-mock-data.ts";

describe("Adatag deployment", async () => {
  test("instantate & deploy", async () => {
    // The envs can be overwritten for dynamic testings, see and example below.
    Bun.env.ENVIRONMENT = "Integration";
    Bun.env.NETWORK = "Custom";
    Bun.env.PROVIDER = "KupmiosV5";

    const { deployerSeed, collectorSeed, userSeed, network, provider } =
      await resolveMockData();

    const translucent = await Translucent.new(provider, network);

    translucent.selectWalletFromSeed(deployerSeed);
    const [utxo] = await translucent.wallet.getUtxos();

    // TODO: Translucent Emulator does not allow paying to different address than the signer.
    const deployerAddress = await addressFromSeed(translucent, deployerSeed);

    // Access the params object for the specified network
    const params = PlutusParams[network];

    // Create a newParams object by spreading the values from the original params
    const testParams = {
      ...params,
      collectorAddress: deployerAddress,
    };

    expect(
      Bootstrap.deploy(translucent, utxo, testParams),
    ).resolves.toBeDefined();
  });
});
