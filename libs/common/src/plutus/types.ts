/**
 * **Val**
 *
 * The `Val` type represents the constraint data as interval set used for storing and validating elements in the tree.
 *
 * In a `IntegriTree`, each element must fall within the initial constraint, forming two nodes
 * that contain the element for every existing element in the tree
 *
 * For any non-existing but valid element (`lower bound < element < upper bound`),
 * there will be only one node. All other elements are considered invalid.
 *
 * This structure ensures the generation of proofs for membership and non-membership of an element.
 *
 * Membership can be validated by:
 * 1. For membership (x ∈ T), if x equals either x' or x" in Val (x', x") of a node of the tree.
 * 2. For non-membership (x ∉ T), if x' < x < x" in Val (x', x") of a node of the tree.
 *
 * All other elements are considered invalid for the tree.
 */
export type Val = { xi: string; xa: string; xb: string }

/**
 * The proof is the minimal subtree of the IntegriTree
 */
export type Proof =
  | {
      HashNode: { hash: string; left: Proof; right: Proof }
    }
  | {
      NodeHash: { hash: string }
    }

/**
 * Redeemer for minting adatag.
 *
 * > Node: The `AdatagAdatagMinting`  interface's rdmr can be set to the MintRedeemer in `plutus.ts`
 * Example of using recursive data type in translucent:
 * ```js
 *  const mintRedeemer: P.MintRedeemer = {
 *     Minting: [
 *       {
 *         updateVal: updateVal, // must be serialised (hex encoded)
 *         appendVal: appendVal,
 *         proof: proof,
 *       },
 *     ], // Use 'as const' to assert that Minting is a tuple with a single element
 *   }
 *
 *   const rdmr = Data.to(mintRedeemer, P.AdatagAdatagMinting.rdmr, 'proof')
 * ```
 */
export type MintRedeemer =
  | {
      Minting: [
        {
          updateVal: Val
          appendVal: Val
          proof: Proof
        }
      ]
    }
    | {
      Burning: [
        {
          updateVal1: Val
          updateVal2: Val
          deleteVal: Val
          proof: Proof
        },
      ]
    }

/**
 * Operation for minting and burning adatag
 */
export type Operation = 'AdatagAdded' | 'AdatagRemoved'

export type TimeDepositDatum = {
  datum: {
    beneficiary: string
    deadLine: bigint
  }
}

export const TimeDepositDatum = Object.assign({
  datum: {
    title: 'TimeDepositDatum',
    anyOf: [
      {
        title: 'TimeDepositDatum',
        dataType: 'constructor',
        index: 0,
        fields: [
          { dataType: 'bytes', title: 'beneficiary' },
          { dataType: 'integer', title: 'deadLine' },
        ],
      },
    ],
  },
}) as unknown as TimeDepositDatum
