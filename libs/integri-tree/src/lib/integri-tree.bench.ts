import { Data, fromText } from 'translucent-cardano'
import { hashVal, rootHash } from '..'
import { IntegriTree } from './integri-tree'
import { sampleVals2 } from './integri-tree.test'
import { AdatagAdatagMinting, MintRedeemer, Proof, Val } from '@adatag/shared/plutus'
import { sampleVals } from './integri-tree.data'

function benchIt(fn: Function) {
  const st = Date.now()
  fn()
  const et = Date.now()
  console.log(`Bench runf ro${fn.name}  in ${et - st} milliseconds.`)
}

function generateRandomStrings(count: number): string[] {
  const maxLength = 15
  const charCodeA = 'a'.charCodeAt(0)
  const charCodeZ = 'z'.charCodeAt(0)

  const randomStrings: string[] = []

  for (let j = 0; j < count; j++) {
    const stringLength = Math.floor(Math.random() * (maxLength + 1))
    const randomStringArray: string[] = []

    for (let i = 0; i < stringLength; i++) {
      const randomCharCode =
        Math.floor(Math.random() * (charCodeZ - charCodeA + 1)) + charCodeA
      const randomChar = String.fromCharCode(randomCharCode)
      randomStringArray.push(randomChar)
    }

    const rs = randomStringArray.join('')
    if (rs === '') {
      continue
    }
    randomStrings.push(randomStringArray.join(''))
  }

  return randomStrings
}

// Benchmark function
function runBenchmark(numberOfStrings: number): string[] {
  const startTime = Date.now()

  // Run the benchmarked function
  const randomStrings = generateRandomStrings(numberOfStrings)

  const endTime = Date.now()
  const elapsedMilliseconds = endTime - startTime

  console.log(
    `Generated ${numberOfStrings} random strings in ${elapsedMilliseconds} milliseconds.`,
  )
  return randomStrings
}

/*
const numberOfStrings = 13;
const rs = runBenchmark(numberOfStrings);

const tree = IntegriTree.fromConstraints("`", "{")

rs.forEach((r) => {
    tree.append(r)
})
*/

// console.log(`Importing....`)
//import { sampleVals } from './integri-tree.data'
//console.log(`Loading from vals`)
//console.log(`Hashing now`)

// DEBUG: 
//const tree = IntegriTree.fromList(sampleVals)
const tree = IntegriTree.fromList(sampleVals2)
//const tree = IntegriTree.fromConstraints("`", "b")
const rh = tree.rootHash()

console.log(`Roothash: ${rh}`)
//console.log(`Tree: ${tree}`)

const adatag = "ubul" // lca === appenNode === updateNode

// const adatag = "adam"\
//console.log(tree.generateMinimalSubtreeOf("ilap"))
//tree.append(adatag)
const st = Date.now()
/*const mintRedeemer: MintRedeemer = tree.generateMinimalSubtree(adatag)
const et = Date.now()
const em = et - st
console.log(`generateProof for ${tree.size} took ${em} milliseconds`)//tree.toList())

if (typeof mintRedeemer !== 'string') {
  const proof: Proof =  mintRedeemer.Minting[0].proof
    
  console.log(`Root hash of proof: ${rootHash(proof)}`)
  console.log(`Proof: ${JSON.stringify(proof)}`)
  console.log(`Root hash of proof: ${rootHash(proof)}`)
}

const rdmr = Data.to(mintRedeemer, AdatagAdatagMinting.rdmr, 'proof')
console.log(rdmr)
*/