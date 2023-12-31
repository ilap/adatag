import { test, expect, describe } from "bun:test";

import {
  Data,
  PrivateKey,
  Translucent,
  fromText,
  generatePrivateKey,
} from "translucent-cardano";
import * as plutus from "../../config/plutus.ts";
import { AppSettings } from "../../config/config.ts";
import { getPolicyId } from "../utils.ts";

describe("Simple auth NFT mint", async () => {
  const user1: PrivateKey = generatePrivateKey();
  const address1: string = await (await Translucent.new(undefined, "Custom"))
    .selectWalletFromPrivateKey(user1)
    .wallet.address();

  const user2: PrivateKey = generatePrivateKey();
  const address2: string = await (await Translucent.new(undefined, "Custom"))
    .selectWalletFromPrivateKey(user2)
    .wallet.address();

  const accounts = [
    { address: address1, assets: { lovelace: 10_000_000_000n } },
    { address: address2, assets: { lovelace: 10_000_000_000n } },
  ];

  let translucent = (await AppSettings.getInstance(accounts)).translucent;

  await translucent.selectWalletFromPrivateKey(user1);
  const [utxo] = await translucent.wallet.getUtxos();
  const txh = utxo.txHash;
  const idx = utxo.outputIndex;

  const mintingPolicy = new plutus.OneshotAuthToken({
    transactionId: { hash: txh },
    outputIndex: BigInt(idx),
  });

  const policyId = await getPolicyId(mintingPolicy);
  //console.log(`policy id: ${policyId}`);

  test("mint auth tokens", async () => {
    const tx = await translucent
      .newTx()
      .mintAssets({ [policyId + fromText("a")]: 1n }, Data.void())
      .attachMintingPolicy(mintingPolicy)
      .complete();

    const signedTx = await tx.sign().complete();
    //console.log(signedTx.txSigned.to_json())

    expect(signedTx.submit()).resolves.toBeDefined();
  });
});
