import { Val } from './plutus_types'


/** 
 * member, nonmember
 * ```js
 * e == xa or e == xb // member
 * xa < e < xb // non-member
 * ```
 */
export const member = (e: string, val:Val) => e == val.xa || e == val.xb
export const nonmember = (e: string, val:Val) => val.xa  < e && e <  val.xb
export const validMembership = (e: string, val: Val) => member(e, val) || nonmember(e, val)
export type { Val }