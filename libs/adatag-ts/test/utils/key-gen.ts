import { Translucent } from 'translucent-cardano'
import { mkdir, stat } from 'node:fs/promises'

const translucent = await Translucent.new(undefined, 'Custom')

async function handleIO(
  dir: string,
  file_name: string,
  generator: () => Promise<string> | string,
): Promise<string> {
  try {
    // Does not exist or no permission to read.
    await stat(dir)
  } catch {
    await mkdir(dir, { recursive: true })
  }
  const path = dir + file_name
  try {
    return await Bun.file(path).text()
  } catch (_) {
    const data = typeof generator === 'function' ? await generator() : generator
    await Bun.write(path, data)
    return data
  }
}

const dir = '../../keys/'
const collector = 'collector'
const deployer = 'deployer'
const user = 'user'

// export const collectorPrivateKey = await handleKey(COLLECTOR_KEY, translucent.utils.select..generatePrivateKey)
// translucent.selectWalletFromPrivateKey(collectorPrivateKey).wallet.address())
const collectorSeed: string = await handleIO(
  dir,
  collector + '.seed',
  translucent.utils.generateSeedPhrase,
)
const collectorAddress: string = await handleIO(dir, collector + '.addr', () =>
  translucent.selectWalletFromSeed(collectorSeed).wallet.address(),
)
//const collectorPkh: string = getAddressDetails(collectorAddress).paymentCredential?.hash || "";

const ownerSeed: string = await handleIO(
  dir,
  deployer + '.seed',
  translucent.utils.generateSeedPhrase,
)
const ownerAddress: string = await handleIO(dir, deployer + '.addr', () =>
  translucent.selectWalletFromSeed(ownerSeed).wallet.address(),
)

const userSeed: string = await handleIO(
  dir,
  user + '.seed',
  translucent.utils.generateSeedPhrase,
)
const userAddress: string = await handleIO(dir, user + '.addr', () =>
  translucent.selectWalletFromSeed(userSeed).wallet.address(),
)

function showDetails() {
  console.log(`Collector address: ${collectorAddress}`)
  console.log(`Collector seed   : ${collectorSeed}`)
  console.log(`-------------------------------------------`)

  console.log(`Deployer address : ${ownerAddress}`)
  console.log(`Deployer seed    : ${ownerSeed}`)
  console.log(`-------------------------------------------`)

  console.log(`Test User address: ${userAddress}`)
  console.log(`Test User seed   : ${userSeed}`)
  console.log(`-------------------------------------------`)
  console.log(
    `Faucet address       : https://docs.cardano.org/cardano-testnet/tools/faucet/`,
  )
}

showDetails()
