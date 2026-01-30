// This file creates some fixtures we can use for testing

use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{PolicyId}

pub const own_hash: PolicyId =
  #"00010203040506070809101112131401020304050607080910111213"

pub const other_hash: PolicyId =
  #"010304050607080910111213141502030405060708091011121314"

pub const another_hash: PolicyId =
  #"02030405060708091011121314150203040506070809101112131415"

pub const beneficiary_keyhash =
  #"03040506070809101112131415160304050607080910111213141516"

pub const collector_keyhash =
  #"04050607080910111213141516170405060708091011121314151617"

pub const other_keyhash =
  #"05060708091011121314150203040506070809101112131415161718"

pub const adversarial_keyhash =
  #"06070809101112131415020304050607080910111213141516171819"

pub const beneficiary_address = keyhash_address(beneficiary_keyhash)

pub const collector_address = keyhash_address(collector_keyhash)

pub const adversarial_address = keyhash_address(adversarial_keyhash)

pub const own_scriptaddress = scripthash_address(own_hash)

pub const other_scriptaddress = scripthash_address(other_hash)

pub const another_scriptaddress = scripthash_address(other_hash)

pub fn keyhash_address(keyhash: ByteArray) {
  Address {
    payment_credential: VerificationKey(keyhash),
    stake_credential: None,
  }
}

pub fn scripthash_address(scripthash: ByteArray) {
  Address { payment_credential: Script(scripthash), stake_credential: None }
}
