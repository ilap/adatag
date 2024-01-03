import { test, expect, describe } from "bun:test";

import { Assets, Translucent, generateSeedPhrase } from "translucent-cardano";

import { mintAdahandle } from "../utils/mock-adahandle.ts";
import { resolveMockData } from "../utils/resolve-mock-data.ts";

async function generateAccount(assets: Assets) {
  const seedPhrase = generateSeedPhrase();
  return {
    seedPhrase,
    address: await (await Translucent.new(undefined, "Custom"))
      .selectWalletFromSeed(seedPhrase)
      .wallet.address(),
    assets,
  };
}

test("minting mock-adahandle", async () => {
  // The envs can be overwritten for dynamic testings, see and example below.
  Bun.env.ENVIRONMENT = "Development";
  Bun.env.NETWORK = "Custom";
  Bun.env.PROVIDER = "Emulator";

  const { deployerSeed, collectorSeed, userSeed, network, provider } =
    await resolveMockData();

  const translucent = await Translucent.new(provider);

  translucent.selectWalletFromSeed(userSeed);
  const address = await translucent.wallet.address();

  expect(mintAdahandle(translucent, ["alma", "korte"], address));
});
