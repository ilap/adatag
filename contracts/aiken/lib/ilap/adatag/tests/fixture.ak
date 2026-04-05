use aiken/crypto.{ScriptHash}
// This file creates some fixtures we can use for testing

use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{PolicyId}

pub const token_policy: PolicyId =
  #"00010203040506070809101112131401020304050607080910111213"

pub const timedeposit_hash: ScriptHash =
  #"010304050607080910111213141502030405060708091011121314"

pub const stateholder_hash: ScriptHash =
  #"02030405060708091011121314150203040506070809101112131415"

pub const minting_policy: PolicyId =
  #"02030405060708091011121314150203040506070809101112131415"

pub const adversarial_policy: PolicyId =
  #"08091011121314150203040506070809101112131415161718192021"

pub const adahandle_policy: PolicyId =
  #"09101112131415020304050607080910111213141516171819202122"

pub const state_holder_address = scripthash_address(stateholder_hash)

pub const timedeposit_address = scripthash_address(timedeposit_hash)

pub const beneficiary_keyhash =
  #"03040506070809101112131415160304050607080910111213141516"

pub const collector_keyhash =
  #"04050607080910111213141516170405060708091011121314151617"

pub const adversarial_keyhash =
  #"06070809101112131415020304050607080910111213141516171819"

pub const user_keyhash =
  #"07080910111213141502030405060708091011121314151617181920"

pub const beneficiary_address = keyhash_address(beneficiary_keyhash)

pub const collector_address = keyhash_address(collector_keyhash)

pub const adversarial_address = keyhash_address(adversarial_keyhash)

pub const user_address = keyhash_address(user_keyhash)

pub fn keyhash_address(keyhash: ByteArray) {
  Address {
    payment_credential: VerificationKey(keyhash),
    stake_credential: None,
  }
}

pub fn scripthash_address(scripthash: ByteArray) {
  Address { payment_credential: Script(scripthash), stake_credential: None }
}
