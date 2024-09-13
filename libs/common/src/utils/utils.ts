import {
  wordlist,
  mnemonicToEntropy,
  Bip32PrivateKey,
  TransactionOutput,
  Transaction,
  TransactionId,
  AddressType,
  NetworkId,
  Address,
} from '@blaze-cardano/core'

import type { Provider } from '@blaze-cardano/query'
import type { Blaze } from '@blaze-cardano/sdk'
import { makeValue } from '@blaze-cardano/tx'
import { HotWallet, type Wallet } from '@blaze-cardano/wallet'
import { Network } from './types'
import { SLOT_CONFIG_NETWORK } from './time'

export function assert(condition: unknown, msg?: string): asserts condition {
  if (condition === false) throw new Error(msg)
}

export function delayRandom(max: number) {
  const ms = Math.random() * max * 1000
  return new Promise(resolve => setTimeout(resolve, ms))
}

export function stringifyData(data: unknown) {
  return JSON.stringify(data, (_, value) => (typeof value === 'bigint' ? value.toString() : value), '  ')
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

export async function setSlotConfig(network: Network, env: string, unixTime?: number) {
  if (network === 'Custom' && env === 'Development') {
    // DEBUG: console.log(`Setting slot config for ${network}... ${unixTime}`)
    SLOT_CONFIG_NETWORK[network] = {
      zeroTime: unixTime ?? 0,
      zeroSlot: 0,
      slotLength: 1000,
    }
  } else if (network === 'Custom' && env === 'Integration') {
    try {
      const response = await fetch(`http://localhost:10000/local-cluster/api/admin/devnet`)
      const serverInfo = await response.json()

      if (serverInfo.startTime !== 0) {
        SLOT_CONFIG_NETWORK[network] = {
          zeroTime: serverInfo.startTime * 1000,
          zeroSlot: 0,
          slotLength: serverInfo.slotLength * 1000,
        }
      }
    } catch (error) {
      throw new Error(`Could not set the slot config`)
    }
  }
  console.log(`SLOT_CONFIG_NETWORK[${network}]: ${JSON.stringify(SLOT_CONFIG_NETWORK[network])}`)
}

function isLetter(letters: string, prefix: string | null) {
  const letter = letters.charCodeAt(0)

  return prefix ? letters[0] === prefix : letter >= 0x61 && letter <= 0x7a
}

export function generateRandomStrings(count: number, maxLength: number, prefix: string | null): Set<string> {
  const letters = 'abcdefghijklmnopqrstuvwxyz'
  const validCharacters = letters + '.-_'

  const randomStrings: Set<string> = new Set()

  for (let j = 0; j < count; j++) {
    const stringLength = Math.floor(Math.random() * (maxLength + 1))
    let str = ''

    for (let i = 0; i < stringLength; i++) {
      const randomIndex = Math.floor(Math.random() * validCharacters.length)
      str += validCharacters.charAt(randomIndex)
    }

    if (str !== '' && isLetter(str, prefix)) {
      randomStrings.add(str)
    }
  }

  return randomStrings
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

// Example usage:

//const randomStrings = generateRandomStrings(30000, 17);
//randomStrings.forEach((name) => {
//  console.log(`"${name}", ${isValidUsername(name)} `)
//})
//console.log(randomStrings);

export function calculateDeposit(adatag: string, depositBase: number, minDeposit: number, maxLength: number): bigint {
  const len = adatag.length

  return len > maxLength
    ? BigInt(minDeposit)
    : BigInt(Math.max(minDeposit, (depositBase / 2 ** (adatag.length - 1)) >> 0))
}

export function generateGenesisOutputs(address: Address, n: number, value: bigint): TransactionOutput[] {
  return Array(n).fill(new TransactionOutput(address, makeValue(value)))
}

export async function generateAccountWithSeed({
  seed,
  addressType = AddressType.BasePaymentKeyStakeKey,
  amount,
}: {
  seed: string
  addressType?: AddressType
  amount: bigint
}) {
  const masterkey = await masterkeyFromMnenomic(seed)

  const { address } = await HotWallet.generateAccountAddressFromMasterkey(masterkey, NetworkId.Testnet, addressType)

  const utxo = generateGenesisOutputs(address, 1, amount)

  return {
    utxo,
    masterkeyHex: masterkey.hex(),
  }
}

export async function masterkeyFromMnenomic(mnemonic: string) {
  const entropy = mnemonicToEntropy(mnemonic, wordlist)
  const buff = Buffer.from(entropy)
  const masterkey = Bip32PrivateKey.fromBip39Entropy(buff, '')
  return masterkey
}

export function toText(hex: string): string {
  return new TextDecoder().decode(fromHex(hex))
}

/** Convert a Utf-8 encoded string to a Hex encoded string. */
export function fromText(text: string): string {
  return toHex(new TextEncoder().encode(text))
}

export function fromHex(hex: string): Uint8Array {
  const matched = hex.match(/.{1,2}/g)
  return new Uint8Array(matched ? matched.map(byte => parseInt(byte, 16)) : [])
}

export function toHex(bytes: Uint8Array): string {
  return Array.from(bytes)
    .map(byte => byte.toString(16).padStart(2, '0'))
    .join('')
}

export async function signAndSubmit(tx: Transaction, blaze: Blaze<Provider, Wallet>): Promise<TransactionId> {
  const signed = await blaze.wallet.signTransaction(tx, true)
  const ws = tx.witnessSet()
  ws.setVkeys(signed.vkeys()!)
  tx.setWitnessSet(ws)
  return await blaze.provider.postTransactionToChain(tx)
}
