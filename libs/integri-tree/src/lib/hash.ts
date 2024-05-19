//import * as P from 'plutus'
import { sha256 } from 'js-sha256'
import { Proof, Val } from '@adatag/common/plutus'

// export const emptyHash = '836cc68931c2e4e3e838602eca1902591d216837bafddfe6f0c8cb07' // Blake2b_224
export const emptyHash = 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855' // SHA2_256

/**
 * The hash of the node's Value is the hash of the concatenated `xi`, `xa` and `xb` of the Val.
 *
 * @param {Val} val - The value object containing `xi`, `xa` and `xb` fields.
 * @returns {string} - The hash of the concatenated `xi`, `xa` and `xb` fields.
 */
export const hashVal = (val: Val): string => hash256(stringToHex(val.xi + val.xa + val.xb))

// FIXME: Buffer is not compatible with browsers Buffer.from(val.xi + val.xa + val.xb, 'utf-8').toString('hex')
/**
 * @param {string} input - The input string to be converted to hexadecimal.
 * @returns {string} - The hexadecimal representation of the input string.
 */
function stringToHex(input: string): string {
  return input
    .split('')
    .map(char => char.charCodeAt(0).toString(16))
    .join('')
}

/**
 * @param {string} msg - The message to be hashed.
 * @returns {string} - The SHA-256 hash of the input message.
 */
export const hash = (msg: string): string => hash256(msg)

/**
 * @param {string} message - The input message to be hashed.
 * @returns {string} - The SHA-256 hash of the input message.
 */
export function hash256(message: string): string {
  const array = message ? new Uint8Array(message.match(/../g)?.map(h => parseInt(h, 16)) || []) : ''
  return sha256(array)
}

/**
 * @param {string} h1 - The first hash input.
 * @param {string} h2 - The second hash input.
 * @param {string} h3 - The third hash input.
 * @returns {string} - The combined hash of the three input hashes.
 */
export function combineThreeHashes(h1: string, h2: string, h3: string): string {
  return hash(h1 + h2 + h3)
}

/**
 * @param {string} h1 - The first hash input.
 * @param {string} h2 - The second hash input.
 * @returns {string} - The combined hash of the two input hashes.
 */
export function combineHashes(h1: string, h2: string): string {
  return hash(h1 + h2)
}

/**
 * @param {Proof} root - The root of the proof tree.
 * @returns {string} - The hash of the root of the proof tree.
 */
export function rootHash(root: Proof): string {
  if ('NodeHash' in root) {
    // If the root is a NodeHash, return its hash
    return root.NodeHash.hash
  } else if ('HashNode' in root) {
    // If the root is a HashNode, combine three hashes
    const { hash, left, right } = root.HashNode
    return combineThreeHashes(hash, rootHash(left), rootHash(right))
  } else {
    throw Error(`Invalid proof: ${root}`)
  }
}
