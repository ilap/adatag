import * as P from '../../common/plutus.ts'
import { createHash } from 'crypto'

// export const emptyHash = '836cc68931c2e4e3e838602eca1902591d216837bafddfe6f0c8cb07' // Blake2b_224
export const emptyHash =
  'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855' // SHA2_256

export type Val = { xi: string; xa: string; xb: string }

export const hashVal = (val: Val): string =>
  hash256(Buffer.from(val.xi + val.xa + val.xb, 'utf-8').toString('hex'))
export const hash = (msg: string): string => hash256(msg)

export function hash256(message: string): string {
  const hash = createHash('sha256')
  hash.update(Buffer.from(message, 'hex'))
  return hash.digest().toString('hex')
}

//export const hash = (message: string): string => hash256(message)
export function combineThreeHashes(h1: string, h2: string, h3: string): string {
  const b1 = Buffer.from(h1, 'hex')
  const b2 = Buffer.from(h2, 'hex')
  const b3 = Buffer.from(h3, 'hex')

  const buff = Buffer.concat([b1, b2, b3]).toString('hex')

  return hash(buff)
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
