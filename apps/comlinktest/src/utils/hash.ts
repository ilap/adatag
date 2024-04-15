import * as P from './plutus_types'
import { sha256 } from 'js-sha256'

// export const emptyHash = '836cc68931c2e4e3e838602eca1902591d216837bafddfe6f0c8cb07' // Blake2b_224
export const emptyHash =
  'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855' // SHA2_256

/**
 * The hash of the node's Value is the hash of the concatenated `xi`, `xa` and `xb` of the Val.
 */
export const hashVal = (val: P.Val): string =>
  hash256(Buffer.from(val.xi + val.xa + val.xb, 'utf-8').toString('hex'))
export const hash = (msg: string): string => hash256(msg)

export function hash256(message: string | null): string {
  const array = message ? new Uint8Array(message.match(/../g).map(h => parseInt(h, 16))) : ''
  return sha256(array)
}

export function combineThreeHashes(h1: string, h2: string, h3: string): string {
  return hash(h1 + h2 + h3)
}

export function combineHashes(h1: string, h2: string): string {
  return hash(h1 + h2)
}

export function rootHash(root: P.Proof): string {
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