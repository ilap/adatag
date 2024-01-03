import { test, expect, describe } from "bun:test";

import { Assets, Data, Translucent, fromText } from "translucent-cardano";

import * as P from "../../common/plutus.ts";
import { resolveMockData } from "../utils/resolve-mock-data.ts";
import { addressFromSeed } from "../utils/utils.ts";

describe("Simple auth NFT mint", async () => {
  //Bun.env.ENVIRONMENT = "Development"
  //Bun.env.NETWORK = "Custom"
  //Bun.env.PROVIDER = "Emulator"
  test("mint auth tokens", async () => {
    const { deployerSeed, collectorSeed, userSeed, network, provider } =
      await resolveMockData();

    const translucent = await Translucent.new(provider);

    translucent.selectWalletFromSeed(deployerSeed);
    const [utxo] = await translucent.wallet.getUtxos();

    console.log(utxo);
    console.log(provider);

    const authMintingPolicy = new P.OneshotAuthToken({
      transactionId: { hash: utxo.txHash },
      outputIndex: BigInt(utxo.outputIndex),
    });

    const policyId = translucent.utils.mintingPolicyToId(authMintingPolicy);

    //const asset: Asset = {} /// [policyId + fromText("a")]: 1n }
    let assets: Assets = {};
    const assetId = policyId + "a";
    assets[assetId] = 1n;

    const userAddress = await addressFromSeed(translucent, userSeed)
    const tx = await translucent
      .newTx()
      .collectFrom([utxo])
      .payToAddress(userAddress, assets )
      .attachMintingPolicy(authMintingPolicy)
      .mintAssets(assets, Data.void())
      .complete();

    console.log( tx.txComplete.to_json());
    const signedTx = await tx.sign().complete();
    console.log(signedTx.txSigned.to_json());

    expect(signedTx.submit()).resolves.toBeDefined();
  });
});


