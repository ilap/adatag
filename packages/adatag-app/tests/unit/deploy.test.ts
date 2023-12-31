import { test, expect, describe } from "bun:test";

import { AppSettings } from "../../config/config.ts";
import {
  Assets,
  Emulator,
  Translucent,
  generateSeedPhrase,
  getAddressDetails,
} from "translucent-cardano";
import { Adatag, UtxoReference } from "../../scripts/application.ts";

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

describe("Adatag deployment", async () => {
  const owner = await generateAccount({ lovelace: 75_000_000_000n });
  const collector = await generateAccount({ lovelace: 100_000_000n });
  const test_user = await generateAccount({ lovelace: 100_000_000n });

  const appSettings = await AppSettings.getInstance([
    owner,
    collector,
    test_user,
  ]);
  const translucent = appSettings.translucent;

  translucent.selectWalletFromSeed(owner.seedPhrase);
  const [utxo] = await translucent.wallet.getUtxos();

  // Set the owner utxo for one shot auth token.
  const outref: UtxoReference = {
    utxoRef: {
      transactionId: { hash: utxo!.txHash },
      outputIndex: BigInt(utxo!.outputIndex),
    },
  };

  const { paymentCredential } = getAddressDetails(collector.address);

  test("instantate", async () => {
    expect(Adatag.getInstance(outref, paymentCredential!, appSettings));
  });

  test("instantate & deploy", async () => {
    const app = await Adatag.getInstance(
      outref,
      paymentCredential!,
      appSettings,
    );
    let txHash = await app.deploy();
    expect(txHash != "");
    //(translucent.provider as Emulator).log()
  });

  test("mint @adatag", async () => {
    const app = await Adatag.getInstance(
      outref,
      paymentCredential!,
      appSettings,
    );
    let txHash = await app.deploy();
    expect(txHash != "");
  });
});
