import {
  Assets,
  MintingPolicy,
  Network,
  SLOT_CONFIG_NETWORK,
  Translucent,
  generateSeedPhrase,
} from 'translucent-cardano'

export function assert(condition: unknown, msg?: string): asserts condition {
  if (condition === false) throw new Error(msg)
}

export function delayRandom(max: number) {
  const ms = Math.random() * max * 1000
  return new Promise(resolve => setTimeout(resolve, ms))
}

export async function generateAccountWithSeed(seed: string, assets: Assets) {
  return {
    seed,
    address: await (await Translucent.new(undefined, 'Custom'))
      .selectWalletFromSeed(seed)
      .wallet.address(),
    assets,
  }
}

export async function generateAccount(assets: Assets) {
  const seed = generateSeedPhrase()
  return generateAccountWithSeed(seed, assets)
}

export async function getPolicyId(mp: MintingPolicy): Promise<string> {
  return (await Translucent.new(undefined, 'Custom')).utils.mintingPolicyToId(
    mp,
  )
}

export function stringifyData(data: unknown) {
  return JSON.stringify(
    data,
    (key, value) => (typeof value === 'bigint' ? value.toString() : value),
    '  ',
  )
}

interface YaciInfo {
  nodePort: number
  submitApiPort: number
  socketPath: string
  protocolMagic: number
  slotLength: number
  blockTime: number
  epochLength: number
  p2pEnabled: boolean
  startTime: number
  masterNode: boolean
  adminNodeUrl: string | null
  ogmiosPort: number
  kupoPort: number
  yaciStorePort: number
  socatPort: number
  blockProducer: boolean
}

export async function setSloctConfig(network: Network, env: string) {
  // Only for
  if (!(network == 'Custom' && env == 'Integration')) {
    return
  }
  try {
    const response = await fetch(
      `http://localhost:10000/local-cluster/api/admin/clusters/default`,
    )

    if (response.ok) {
      const serverInfo: YaciInfo = await response.json()
      if (serverInfo.startTime !== 0) {
        SLOT_CONFIG_NETWORK[network] = {
          zeroTime: serverInfo.startTime * 1000,
          zeroSlot: 0,
          slotLength: serverInfo.slotLength * 1000,
        }
      }
    } else {
      throw Error(`Could not set the slot config`)
    }
  } catch (error) {
    throw Error(`Could not set the slot config`)
  }
}
//
function isLetter(letters: string, prefix: string | null) {
  const letter = letters.charCodeAt(0)

  return prefix ? letters[0] === prefix : letter >= 0x61 && letter <= 0x7a
}

export function generateRandomStrings(count: number, maxLength: number, prefix: string | null): Set<string> {
  const letters = 'abcdefghijklmnopqrstuvwxyz'
  const validCharacters = letters + '.-_';

  const randomStrings: Set<string> = new Set();

  for (let j = 0; j < count; j++) {
    const stringLength = Math.floor(Math.random() * (maxLength + 1));
    let str = '';

    for (let i = 0; i < stringLength; i++) {
      const randomIndex = Math.floor(Math.random() * validCharacters.length);
      str += validCharacters.charAt(randomIndex);
    }

    if (str !== '' && isLetter(str,prefix)) {
      randomStrings.add(str);
    }
  }

  return randomStrings;
}


export function isValidUsername(str: string): boolean {
  const n = str.length;

  return (
    n > 0 &&
    n <= 16 &&
    isLowerCase(str.charCodeAt(0)) &&
    isLowerCaseOrDigit(str.charCodeAt(n - 1)) &&
    hasOnlyAllowedChars(str)
  );
}

export function isLowerCase(ch: number): boolean {
  return ch >= 97 && ch <= 122;
}

function isDigit(digit: number): boolean {
  return digit >= 48 && digit <= 57;
}

function isLowerCaseOrDigit(ch: number): boolean {
  return isLowerCase(ch) || isDigit(ch);
}

function isHyphen(char: number): boolean {
  return char === 45;
}

function isUnderscore(char: number): boolean {
  return char === 95;
}

function isDot(char: number): boolean {
  return char === 46;
}

function isValidChar(ch: number): boolean {
  return isLowerCase(ch) || isDigit(ch) || isHyphen(ch) || isUnderscore(ch) || isDot(ch);
}

function hasOnlyAllowedChars(str: string): boolean {
  const n = str.length;

  function go(i: number): boolean {
    if (i >= n) {
      return true;
    }
    if (!isValidChar(str.charCodeAt(i))) {
      return false;
    }
    return go(i + 1);
  }

  return go(0);
}

// Example usage:

//const randomStrings = generateRandomStrings(30000, 17);
//randomStrings.forEach((name) => {
//  console.log(`"${name}", ${isValidUsername(name)} `)
//})
//console.log(randomStrings);
