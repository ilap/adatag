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
 * member, nonmember
 * ```js
 * e == xa or e == xb
 * xa < e < xb
 * ```
 */
export const member = (e: string, val:Val) => e == val.xa || e == val.xb
export const nonmember = (e: string, val:Val) => val.xa  < e && e <  val.xb
export const validMembership = (e: string, val: Val) => member(e, val) || nonmember(e, val)

/**
 * IntegriTree
 *
 * The 'IntegriTree' is a Complete Binary Tree-based data structure designed for verifying data integrity,
 * similar to Merkle trees. It includes proofs for membership, non-membership, and updates (insertation into
 * and removal from the tree).
 * It uses open interval for enforce uniqueness of the elements in the tree.
 */
export type IntegriTree =
  | { Leaf: string }
  | { Node: { val: Val; left: IntegriTree; right: IntegriTree } }
