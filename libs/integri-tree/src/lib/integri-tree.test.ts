import { IntegriTree } from './integri-tree'
import { Val } from './types'


/*const tree = IntegriTree.fromLetter('a')
console.log(`Hash initial: ${tree.rootHash()}`)
console.log(`${JSON.stringify(tree)}`)
//tree.append('ilap')
tree.append('adam')
console.log(`Hash adam  : ${tree.rootHash()}`)
console.log(tree)
tree.delete('adam')
console.log(`Hash adam  : ${tree.rootHash()}`)
console.log(tree.toString())
console.log(tree.findLca(0, 0, 1))
*/
export const sampleVals2: Val[] = [
  { xi: '0', xa: '`', xb: 'c' },
  { xi: '1', xa: 'z', xb: '{' },
  { xi: '2', xa: 'f', xb: 'g' },
  { xi: '3', xa: 'j', xb: 'l' },
  { xi: '4', xa: 'd', xb: 'e' },
  { xi: '5', xa: 'y', xb: 'z' },
  { xi: '6', xa: 'g', xb: 'h' },
  { xi: '7', xa: 'l', xb: 'o' },
  { xi: '8', xa: 'u', xb: 'ul' },
  { xi: '9', xa: 's', xb: 'u' },
  { xi: '10', xa: 'x', xb: 'y' },
  { xi: '11', xa: 'e', xb: 'f' },
  { xi: '12', xa: 'o', xb: 's' },
  { xi: '13', xa: 'i', xb: 'j' },
  { xi: '14', xa: 'v', xb: 'x' },
  { xi: '15', xa: 'c', xb: 'd' },
  { xi: '16', xa: 'h', xb: 'i' },
  { xi: '17', xa: 'ul', xb: 'v' }, //change 9 to Val 9 "u", xb: "ul"
]
/*
const tree2 = IntegriTree.fromList(sampleVals2)

console.log(tree2.findLca(0, 9, 10))
console.log(tree2.toList())
console.log(tree2.rootHash())
*/