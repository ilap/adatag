import { emptyHash, hashVal, combineThreeHashes } from './hash'
import { Proof } from '@adatag/shared/plutus'

import { Val } from './types'

/**
 * @module IntegriTree
 * @description Complete Binary Tree-based data structure for data integrity verification.
 * @license GPL-3
 *
 * The 'IntegriTree' module provides a data structure designed for verifying data integrity, akin to Merkle trees.
 * It offers proofs for membership, non-membership, and updates (insertion and removal).
 *
 * The tree enforces uniqueness using open intervals.
 *
 * Key Features:
 * - Efficient Insertion: O(1) time complexity in a complete binary tree.
 * - Construction Complexity:
 *   1. Constructing the tree from a list or array of Vals has O(n) time complexity.
 *   2. Constructing the tree from a list or array of elements has O(n*m) complexity.
 * - Data Integrity Verification: Provides advantages for data integrity verification through proofs.
 *
 */
export class IntegriTree {
  /**
   * The array model of tree.
   */
  private elements: Val[]

  /**
   * Generates the root hash for the entire tree.
   */
  public rootHash = (): string => this.calculateNodeHash(0)

  private isEven = (num: number) => num % 2 === 0

  /**
   * compares to Vals based on their index
   */
  private isEqual = (index1: number, index2: number) =>
    this.elements[index1].xi === this.elements[index2].xi &&
    this.elements[index1].xa === this.elements[index2].xa &&
    this.elements[index1].xb === this.elements[index2].xb

  /**
   * Gets the size of the tree.
   */
  get size(): number {
    return this.elements.length
  }

  get numberOfElements(): number {
    return this.size - 1
  }

  private constructor(elements: Val[]) {
    this.elements = elements
  }

  /**
   * Creates an IntegriTree based on specified constraints.
   */
  public static fromConstraints(
    lowerBound: string,
    upperBound: string,
  ): IntegriTree {
    const xs = [{ xi: '0', xa: lowerBound, xb: upperBound }]
    return new IntegriTree(xs)
  }

  /**
   * Creates an IntegriTree with a single letter as the root.
   */
  public static fromLetter(letter: string): IntegriTree {
    if (!/^[a-z]$/.test(letter)) {
      throw Error(
        'The initialization string must be a lower case letter and length 1',
      )
    }
    const cc = letter.charCodeAt(0)
    const xs = [
      {
        xi: '0',
        xa: String.fromCharCode(cc - 1),
        xb: String.fromCharCode(cc + 1),
      },
    ]
    return new IntegriTree(xs)
  }

  /**
   * Creates an IntegriTree from a list of elements.
   */
  public static fromList(elements: Val[]): IntegriTree {
    return new IntegriTree(elements)
  }

  /**
   * Retrieves the list representation of the tree.
   */
  public toList() {
    return this.elements
  }

  /**
   * Checks if the tree contains a specified element.
   * @param element - The element to check for.
   * @returns `true` if the element is found; otherwise, `false`.
   */
  contains(element: string) {
    return this.search(element).length === 2
  }
  /**
   * Finds the indexes of the valid Vals for the specified element.
   * @param element - The element to search for.
   * @returns An array containing the indexes of the valid Vals.
   *
   * Valid when `x = x'` or `x = x"` of the `Val (x', x")` when `x ∈ T`, or `x' < x < x"` when `x ∉ T`
   * Time complexity of search is O(n), as the the tree contains unordered elements.
   */
  private search(element: string): number[] {
    const result: number[] = []

    for (let i = 0; i < this.elements.length; i++) {
      const current = this.elements[i]

      if (current.xa === element || current.xb === element) {
        // Case 1: Element is a member, xa or xb matches
        result.push(i)
        if (result.length === 2) {
          // REVIEW: this should always be sorted
          result.sort()
          break
        }
      } else if (current.xa < element && element < current.xb) {
        // Case 2: Element is not a member, but it falls within the open interval
        result.push(i)
        break // Terminate the search
      }
    }

    return result
  }

  /**
   * Appends an element to the tree.
   * @param element - The element to append.
   * @returns `true` if the element is successfully appended; otherwise, `false`.
   *
   * > **Note**: It does not enforce the constraint on the element (adatag validity check)
   * , as the on-chain code would not validate it anyway due to the different root hashes.
   * The reason is that we need a fast append/delete function for handling million adatags off-chain.
   */
  append(element: string): boolean {
    const [a, b] = this.search(element)
    if (!b) {
      this.elements.push({
        xi: this.elements.length.toString(),
        xa: element,
        xb: this.elements[a].xb,
      })
      this.elements[a].xb = element
    } else {
      return false // Element already exists
    }
    return true
  }

  /**
   * Deletes an element from the tree.
   * @param element - The element to delete.
   * @returns `true` if the element is successfully deleted; otherwise, `false`.
   *
   * > Note: It does not enforce the constraint on the element nor checking the integrity of the
   * found Vals, as the on-chain code would fail anyway.
   */
  delete(element: string): boolean {
    const [a, b] = this.search(element)
    if (b) {
      if (this.elements[a].xa === element) {
        this.elements[a].xa = this.elements[b].xa
      } else {
        this.elements[a].xb = this.elements[b].xb
      }

      const deleted = this.elements.pop() as Val

      // `b` is an index, therefore when the new size === b, then the `this.elements[b]` was the last element.
      if (b !== this.size) {
        this.elements[b].xa = deleted.xa
        this.elements[b].xb = deleted.xb
      }
    } else {
      return false // The element does not exist.
    }
    return true
  }

  /**
   * Calculates the hash for a specified node in the tree.
   * @param index - The index of the node.
   * @returns The hash for the specified node.
   */
  private calculateNodeHash(index: number): string {
    if (index >= this.elements.length) {
      return emptyHash
    }

    const val = this.elements[index]
    const leftChildIndex = 2 * index + 1
    const rightChildIndex = 2 * index + 2

    const valHash = hashVal(val)
    const leftHash = this.calculateNodeHash(leftChildIndex)
    const rightHash = this.calculateNodeHash(rightChildIndex)

    return combineThreeHashes(valHash, leftHash, rightHash)
  }

  /**
   * Finds the lowest common ancestor of two nodes in the tree.
   * @param root - The index of the root in the tree.
   * @param p - The index of the first node.
   * @param q - The index of the second node.
   * @returns The lowest common ancestor of the two nodes.
   */
  public findLca(root: number, p: number, q: number): number {
    if (root == null || root >= this.size) {
      return null
    }

    if (this.isEqual(root, p) || this.isEqual(root, q)) {
      return root
    }

    const left = this.findLca(root * 2 + 1, p, q)
    const right = this.findLca(root * 2 + 2, p, q)

    if (left != null && right != null) {
      return root
    }
    return left != null ? left : right
  }

  /**
   * Generates the minimal subtree (the proof tree) for a specified element.
   * @param element - The element for which to generate the minimal subtree.
   * @returns A Proof or `null` if the operation fails.
   *
   */
  public generateMinimalSubtree(element: string): Proof | null {
    const [updateNode, deleteNode] = this.search(element)

    if (deleteNode) {
      // Generate proof for element deletion is not implemented yet
      console.error(`The element (${element}) is already in the tree`)
      return null
    }

    const appendNode = this.size === 1 ? 0 : Math.floor(this.size / 2) - 1

    // The index of LCA is always smaller or equal to the nodes' index
    const lca =
      updateNode === appendNode
        ? updateNode
        : this.findLca(0, updateNode, appendNode)

    if (updateNode === lca) {
      // update node is the parent of the append node, so one traversal from append node is enough
      const node: Proof = this.buildProofNode(appendNode)
      return this.buildProof(appendNode, 0, node)
    } else if (appendNode === lca) {
      // append node is the parent of the update node, so one traversal from update node is enough
      const node: Proof = this.buildProofNode(updateNode)
      return this.buildProof(updateNode, 0, node)
    } else {
      // neither of them is a parent of the other, so both nodes must traversal to the lca,
      // then select their position in the LCA, and finally
      // traversal from the LCA to the root.
      const un = this.buildProofNode(updateNode)
      const an = this.buildProofNode(appendNode)
      const lcaUn = this.buildProof(updateNode, lca, un)
      const lcaAn = this.buildProof(updateNode, lca, an)

      if ('HashNode' in lcaUn && 'HashNode' in lcaAn) {
        const { hash, left, right } = lcaUn.HashNode

        const lcaLeft =
          'NodeHash' in left ? lcaAn.HashNode.left : lcaUn.HashNode.left
        const lcaRight =
          'NodeHash' in right ? lcaAn.HashNode.right : lcaUn.HashNode.right

        const lcaNode: Proof = {
          HashNode: {
            hash: hash,
            left: lcaLeft,
            right: lcaRight,
          },
        }

        return this.buildProof(lca, 0, lcaNode)
      } else {
        return null // Invalid tree
      }
    }
  }

  /**
   * Builds a Proof Node for a specified index.
   * @param index - The index of the node.
   * @returns The constructed Proof Node.
   */
  private buildProofNode(index: number): Proof {
    const vh = hashVal(this.elements[index])
    // Left child's hash
    const leftHash = this.calculateNodeHash(2 * index + 1)
    // Right child's hash
    const rightHash = this.calculateNodeHash(2 * index + 2)
    const node: Proof = {
      HashNode: {
        hash: vh,
        left: { NodeHash: { hash: leftHash } },
        right: { NodeHash: { hash: rightHash } },
      },
    }

    return node
  }

  /**
   * Builds a Proof for a specified range of nodes in the tree.
   * @param fromIndex - The index of the starting node.
   * @param toIndex - The index of the ending node.
   * @param node - The Proof Node for the starting node.
   * @returns The constructed Proof for the specified range.
   */
  private buildProof(fromIndex: number, toIndex: number, node: Proof): Proof {
    if (fromIndex <= toIndex) {
      return node
    }

    const parentIndex = Math.floor((fromIndex - 1) / 2)

    const vh = hashVal(this.elements[parentIndex])
    // When the fromIndex is even then it's the right one (as the index starts from `0`)
    const { left, right } = this.isEven(fromIndex)
      ? {
          left: { NodeHash: { hash: this.calculateNodeHash(fromIndex - 1) } },
          right: node,
        }
      : {
          left: node,
          right: { NodeHash: { hash: this.calculateNodeHash(fromIndex + 1) } },
        }

    const parentNode: Proof = {
      HashNode: { hash: vh, left: left, right: right },
    }

    return this.buildProof(parentIndex, toIndex, parentNode)
  }
}