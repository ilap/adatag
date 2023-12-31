import { AppSettings } from "../config/config.ts";
import { Assets, Translucent, generateSeedPhrase, getAddressDetails } from "translucent-cardano";
import { Adatag, UtxoReference } from "./application.ts";


async function generateAccount(assets: Assets) {
    const seedPhrase = generateSeedPhrase();
    return {
      seedPhrase,
      address: await (await Translucent.new(undefined, "Custom"))
        .selectWalletFromSeed(seedPhrase).wallet.address(),
      assets,
    };
  }

const OWNER = await generateAccount({ lovelace: 75_000_000_000n });
const COLLECTOR = await generateAccount({ lovelace: 100_000_000n });
const USER = await generateAccount({ lovelace: 100_000_000n });

const appSettings =  await AppSettings.getInstance([OWNER, COLLECTOR, USER])
const translucent = appSettings.translucent;


translucent.selectWalletFromSeed(OWNER.seedPhrase);
const [utxo] = await translucent.wallet.getUtxos();


// Set the owner utxo for one shot auth token.
const outref: UtxoReference = {
    utxoRef: {
        transactionId: { hash: utxo!.txHash },
        outputIndex: BigInt(utxo!.outputIndex)
    }
};

const { paymentCredential } = getAddressDetails(COLLECTOR.address);

const app = await Adatag.getInstance(outref, paymentCredential!, appSettings)

console.log(app)

let txHash = await app.deploy()
console.log(`Deployed, txHash: ${txHash}`);