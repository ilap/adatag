import {
  Assets,
  MintingPolicy,
  Translucent,
  generateSeedPhrase,
} from "translucent-cardano";

export function assert(condition: unknown, msg?: string): asserts condition {
  if (condition === false) throw new Error(msg);
}

export async function generateAccountWithSeed(seed: string, assets: Assets) {
  return {
    seed,
    address: await (await Translucent.new(undefined, "Custom"))
      .selectWalletFromSeed(seed)
      .wallet.address(),
    assets,
  };
}

export async function generateAccount(assets: Assets) {
  const seed = generateSeedPhrase();
  return generateAccountWithSeed(seed, assets);
}

export async function addressFromSeed(translucent: Translucent, seed: string) {
  return await translucent
    .selectWalletFromSeed(seed)
    .wallet.address();
}

export async function getPolicyId(mp: MintingPolicy): Promise<string> {
  return (await Translucent.new(undefined, "Custom")).utils.mintingPolicyToId(
    mp,
  );
}
