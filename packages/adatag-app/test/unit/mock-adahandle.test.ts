import { test, expect } from "bun:test";
import { Translucent } from "translucent-cardano";
import { mintAdahandle } from "../utils/mock-adahandle.ts";
import { resolveMockData } from "../utils/resolve-mock-data.ts";

test("minting mock-adahandle", async () => {
  // The envs can be overwritten for dynamic testings, see and example below.
  //Bun.env.ENVIRONMENT = "Development";
  //Bun.env.NETWORK = "Custom";
  //Bun.env.PROVIDER = "Emulator";

  const { deployerSeed, collectorSeed, userSeed, network, provider } =
    await resolveMockData();

  const translucent = await Translucent.new(provider, network);

  translucent.selectWalletFromSeed(userSeed);
  const address = await translucent.wallet.address();

  expect(
    mintAdahandle(translucent, ["ilap", "pali"], address),
  ).resolves.toBeDefined();
});
