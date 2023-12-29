import { Translucent, KupmiosV5, Kupmios, Network } from "translucent-cardano";

const seed = await Bun.file("./secrets/collector.seed").text()

export const CONFIG = {
    KUPO_PREVIEW_URL: process.env.KUPO_PREVIEW_URL ?? "http://localhost:1442",
    KUPO_PREVIEW_APIKEY: process.env.KUPO_PREVIEW_APIKEY ?? "",
  
    OGMIOS_PREVIEW_URL: process.env.OGMIOS_PREVIEW_URL ?? "http://localhost:1337",
    OGMIOS_PREVIEW_APIKEY: process.env.OGMIOS_PREVIEW_APIKEY ?? "",
 
    USE_OGMIOS_V5: true // process.env.USE_OGMIOS_V5 === "1",
};

export async function setupTestInstance({
  accountIndex = 0,
  network = "Preview",
}: {
  accountIndex: number;
  network?: Network,
}) {
  if (!CONFIG.KUPO_PREVIEW_URL || !CONFIG.OGMIOS_PREVIEW_URL) {
    throw new Error("Missing KUPO_URL or OGMIOS_URL environment variable");
  }
  const provider = CONFIG.USE_OGMIOS_V5
    ? new KupmiosV5(CONFIG.KUPO_PREVIEW_URL, CONFIG.OGMIOS_PREVIEW_URL)
    : new Kupmios(CONFIG.KUPO_PREVIEW_URL, CONFIG.OGMIOS_PREVIEW_URL);

  const translucent = await Translucent.new(provider, network);
  if (!seed) {
    throw new Error("Missing seed environment variable");
  }
  translucent.selectWalletFromSeed(seed, { accountIndex });
  const address = await translucent.wallet.address();
  const details = translucent.utils.getAddressDetails(address);
  const utxos = await translucent.wallet.getUtxos();
  return { translucent, address, utxos, details };
}