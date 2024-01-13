//// 
//// The `TimeDepositDatum`
////
//// Note:  The minting policy will validate the deadline at minting time.
////

use aiken/hash
use aiken/time.{PosixTime}
use aiken/transaction/credential.{Script, VerificationKey}
use ilap/integri_tree/types.{Proof, Val}

pub type TokenName =
  ByteArray

pub type ScriptHash =
  hash.Hash<hash.Sha2_256, Script>

pub type Credential =
  hash.Hash<hash.Sha2_256, VerificationKey>

pub type Hash =
  hash.Hash<hash.Sha2_256, ByteArray>

//-----------------------------------------------------------------------------
//-----------------------// 2. TimeDeposit types -------------------------
pub type TimeDepositRedeemer {
  Collect
  Redeem
}

pub type TimeDepositDatum {
  // Beneficiary of the locked time deposit. It can be any payment address
  beneficiary: Credential,
  // Deadline to claim deposits. 
  // Preferably ~20 days (minting policy's parameter) from the date of the 
  // submitted adatag minting transaction
  deadline: PosixTime,
}

pub type TimeDepositParams {
  // Collecting donations (void datum is used) and unclaimed deposits (after a year or two)
  collector: Credential,
  // The time the collector can collect the donations and the unclaimed time-lock deposits. 
  // It should be twice as deactivation time. ~1-2yrs
  collection_time: PosixTime,
}

//-----------------------------------------------------------------------------
//-----------------------// 3. StateHolder types -------------------------

pub type OperationType {
  AdatagAdded
  AdatagRemoved
}

// Initial 

pub type TreeState {
  operation_count: Int,
  // The operation performed on the  Tree. 
  // Initial operation is AdatagRemove, as the old state's operation will never be
  // validated, and the tree size can go to 0 only when an element is removed (e.g. from size 1)
  operation: OperationType,
  // The adatag added or removed from the tree
  adatag: ByteArray,
  // The size of a tree is the same as the number of adatags in the tree.
  // it represented as ByteArray for comparing with Val's index (which is a ByteArray)
  size: ByteArray,
  // The root hash of the tree
  root_hash: Hash,
  // Corresponding adatag minting policy. It is used to avoid circular dependency
  // between the StateHolder and minting policy.
  minting_policy: ScriptHash,
}

//-----------------------------------------------------------------------------
//-----------------------// 4. Minting Types -------------------------

// The POSIXTime (in ms) until the Lock Time Deposit Feature is active
// (e.g. ~6-9 months form the @adatag bootstrapping).
// The users deposit locked in days, if the feature is active. Preferably 20 days.
// The user's `deadline` in the datum - calculated by the front-end is currentTime + userDepositLockingDays * dayInseconds
// Note: the script would fail if the user's lock end date time in the output datum is larger than the currentTime + 2 * userDepositLockingDays * 86400
// to prevent any accidental over locking.
// The user's min deposit for locking an @adatag.  locking_day: Int,
//timelock_expiry: PosixTime,
// The deposint base from which the user's deposit is calculated.
// deposit_base: Int,
pub type MintParams {
  // The authorization token that's carrying the state of the @adatag tree.
  // AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
  auth_token: ScriptHash,
  // State holder validator, the authorization token and the state reside on.
  state_holder: ScriptHash,
  // TimeDeposit validator, for checking time-lock deposits' outputs.
  timedeposit_validator: ScriptHash,
  // The date when the time lock deposit will be deactivated.
  deactivation_time: PosixTime,
  // The deposit is calculated based on the deposit base and the length of the @adatag being minted.
  deposit_base: Int,
  // Duration in milliseconds representing the waiting period for users to claim their time-locked deposit
  // after the corresponding @adatag is minted.
  locking_days: PosixTime,
  // TODO: Consider allowing not locking when adahandle is present in the output.
  // For bypassing the time-lock output requirement when the user own's and $adahandle same with the @adatag being created
  adahandle: ScriptHash,
}

// The users can mint new usernames or burn theirs.
// An update action could be considered when the users allows changing it's data hash or similar.
// Only if, when the Tag is modified to contain an additional 32-byte long data.
// This, could be used for holding a public key hash or similar arbitrary data.
// Note: Not implemented in this version of @adatag.
pub type MintAction {
  Minting(MintRedeemer)
  Burning
}

// The users can only mint or burn usernames based on the redeemer.
pub type MintRedeemer {
  //adatag: ByteArray, It must be already presented in the state (datum) and minting value
  update_val: Val,
  append_val: Val,
  proof: Proof,
}
