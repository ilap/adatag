import { test, expect, describe } from "bun:test";

import { Data, Translucent, fromText } from "translucent-cardano";
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

    // Always apply network to the constructor.
    const translucent = await Translucent.new(provider, network);

    translucent.selectWalletFromSeed(deployerSeed);
    const [utxo] = await translucent.wallet.getUtxos();

    const userAddress = await addressFromSeed(translucent, deployerSeed);

    const authMintingPolicy = new P.OneshotAuthToken({
      transactionId: { hash: utxo.txHash },
      outputIndex: BigInt(utxo.outputIndex),
    });

    const policyId = translucent.utils.mintingPolicyToId(authMintingPolicy);

    const mintVal = { [policyId + fromText("a")]: 1n };
    const tx = await translucent
      .newTx()
      .collectFrom([utxo])
      // TODO: Translucent does not allow paying to different address than the signer.
      .payToAddress(userAddress, mintVal)
      .attachMintingPolicy(authMintingPolicy)
      .mintAssets(mintVal, Data.void())
      .complete();

    const signedTx = await tx.sign().complete();

    expect(signedTx.submit()).resolves.toBeDefined();
  });
});
