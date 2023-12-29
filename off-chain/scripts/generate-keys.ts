import { Translucent, getAddressDetails } from "translucent-cardano";
import { NETWORK, SECRETS_DIR, COLLECTOR_SEED, COLLECTOR_ADDR } from "../config/constants.ts"
const { mkdir } = require('node:fs/promises');

const translucent = await Translucent.new(undefined, NETWORK)

async function handleIO(dir: string, path: string, generator: () => Promise<string> | string): Promise<string> {
  if (!Bun.file(dir).exists()) {
    await mkdir(SECRETS_DIR, { recursive: true })
  }
  try {
    return await Bun.file(dir + path).text()
  } catch (_) {
    const data = typeof generator === "function" ? await generator() : generator
    await Bun.write(path, data)
    return data
  }
}

// export const collectorPrivateKey = await handleKey(COLLECTOR_KEY, translucent.utils.select..generatePrivateKey)
// translucent.selectWalletFromPrivateKey(collectorPrivateKey).wallet.address())
const collectorSeed: string = await handleIO(SECRETS_DIR, COLLECTOR_SEED, translucent.utils.generateSeedPhrase)
const collectorAddress: string = await handleIO(SECRETS_DIR, COLLECTOR_ADDR, () => translucent.selectWalletFromSeed(collectorSeed).wallet.address())
const collectorPkh: string = getAddressDetails(collectorAddress).paymentCredential?.hash || "";

function showDetails() {
  console.log(`Network              : ${NETWORK}`);
  console.log(`Collector address    : ${collectorAddress}`);
  console.log(`Coll. pubkey hash    : ${collectorPkh}`);

  // @ts-ignore as NETWORK is manually adjusted now.
  if (NETWORK === "Mainnet") {
    console.log(`Faucet address       : https://docs.cardano.org/cardano-testnet/tools/faucet/`);
  }
  console.log(`Check address        : https://${NETWORK.toLowerCase()}.cexplorer.io/${collectorAddress}`);
}

export {
  showDetails,
  collectorAddress,
  collectorSeed,
  collectorPkh,
}