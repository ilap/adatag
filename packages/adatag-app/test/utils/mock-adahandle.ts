import * as P from "../../common/plutus.ts";
import {
  Address,
  Assets,
  Data,
  Emulator,
  Translucent,
  generateSeedPhrase,
} from "translucent-cardano";

export function getAdahandleScript() {
  return new P.AlwaysMintMint();
}

export function getAdahandlePolicyId(translucent: Translucent) {
  const adahandlePolicy = getAdahandleScript();
  return translucent.utils.mintingPolicyToId(adahandlePolicy);
}

export async function mintAdahandle(
  translucent: Translucent,
  adatags: string[],
  toAddres: Address,
) {
  const adahandlePolicy = getAdahandleScript();
  const policyId = getAdahandlePolicyId(translucent);

  let assets: Assets = {};
  adatags.forEach((adatag) => {
    const assetId = policyId + adatag;
    assets[assetId] = 1n;
  });

  const tx = await translucent
    .newTx()
    .payToAddress(toAddres, assets)
    .mintAssets(assets, Data.void())
    .attachMintingPolicy(adahandlePolicy)
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();
  return await translucent.awaitTx(txHash);
}
