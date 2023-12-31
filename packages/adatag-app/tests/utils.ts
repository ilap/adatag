import { MintingPolicy, Translucent } from "translucent-cardano";

export function assert(condition: unknown, msg?: string): asserts condition {
  if (condition === false) throw new Error(msg);
}

export async function getPolicyId(mp: MintingPolicy): Promise<string> {
  return (await Translucent.new(undefined, "Custom")).utils.mintingPolicyToId(
    mp,
  );
}
