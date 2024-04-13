import { Network, SLOT_CONFIG_NETWORK } from 'translucent-cardano'
import { DEBUG } from './configs/settings.ts'

export const hexToASCII = (hex: string) => {
  return hex
    .match(/.{1,2}/g)!
    .map((byte: string) => String.fromCharCode(parseInt(byte, 16)))
    .join('')
}

export function stringifyData(data: unknown) {
  return JSON.stringify(
    data,
    (key, value) => (typeof value === 'bigint' ? value.toString() : value),
    '  '
  )
}

export function debugMessage(message: string) {
  if (DEBUG) {
    console.warn(message)
  }
}

export function isValidUsername(str: string): boolean {
  const n = str.length

  return (
    n > 0 &&
    n <= 16 &&
    isLowerCase(str.charCodeAt(0)) &&
    isLowerCaseOrDigit(str.charCodeAt(n - 1)) &&
    hasOnlyAllowedChars(str)
  )
}

export function isLowerCase(ch: number): boolean {
  return ch >= 97 && ch <= 122
}

function isDigit(digit: number): boolean {
  return digit >= 48 && digit <= 57
}

function isLowerCaseOrDigit(ch: number): boolean {
  return isLowerCase(ch) || isDigit(ch)
}

function isHyphen(char: number): boolean {
  return char === 45
}

function isUnderscore(char: number): boolean {
  return char === 95
}

function isDot(char: number): boolean {
  return char === 46
}

function isValidChar(ch: number): boolean {
  return isLowerCase(ch) || isDigit(ch) || isHyphen(ch) || isUnderscore(ch) || isDot(ch)
}

function hasOnlyAllowedChars(str: string): boolean {
  const n = str.length

  function go(i: number): boolean {
    if (i >= n) {
      return true
    }
    if (!isValidChar(str.charCodeAt(i))) {
      return false
    }
    return go(i + 1)
  }

  return go(0)
}

export const calculateDeposit = (
  adatag: string,
  depositBase: number,
  minDeposit: number,
  maxLength: number
): bigint => {
  const len = adatag.length

  return len > maxLength
    ? BigInt(minDeposit)
    : BigInt(Math.max(5, (depositBase / 2 ** (adatag.length - 1)) >> 0))
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
      // FIXME: it's using cors-anywhere atm.`http://localhost:8901/http://localhost:10000/local-cluster/api/admin/clusters/default`
      `http://localhost:10000/local-cluster/api/admin/clusters/default`
    )

    if (response.ok) {
      const res = await response.json()
      console.warn(`RESULT: ${stringifyData(res)}`)
      const serverInfo: YaciInfo = res
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

export function a2h(str: string) {
  const arr1 = []
  for (let n = 0, l = str.length; n < l; n++) {
    const hex = Number(str.charCodeAt(n)).toString(16)
    arr1.push(hex)
  }
  return arr1.join('')
}
