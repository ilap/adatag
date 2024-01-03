import { test, expect, describe } from "bun:test";
import { Translucent, getAddressDetails } from "translucent-cardano";
import * as PlutusParams from "../../config/plutus-params.json";
import { Bootstrap } from "../../deploy/bootstrap.ts";
import { addressFromSeed, generateAccount } from "../utils/utils.ts";
import { resolveMockData } from "../utils/resolve-mock-data.ts";

describe("Adatag deployment", async () => {
  // The envs can be overwritten for dynamic testings, see and example below.
  Bun.env.ENVIRONMENT = "Integration"
  Bun.env.NETWORK = "Custom"
  Bun.env.PROVIDER = "KupmiosV5"

  const { deployerSeed, collectorSeed, userSeed, network, provider } =
    await resolveMockData();

  const translucent = await Translucent.new(provider);

  translucent.selectWalletFromSeed(deployerSeed);
  const [utxo] = await translucent.wallet.getUtxos();

  const collAddress = await addressFromSeed(translucent, collectorSeed);

  // Access the params object for the specified network
  const params = PlutusParams[network];

  // Create a newParams object by spreading the values from the original params
  const testParams = {
    ...params,
    collectorAddress: collAddress,
  };

  test("instantate & deploy", async () => {
    expect(Bootstrap.deploy(translucent, utxo, testParams));
  });
});
