// This file creates some fixtures we can use for testing

use aiken/transaction/credential.{
  Address, ScriptCredential, VerificationKeyCredential,
}

pub const own_hash = #"01020304050607080910111213140102030405060708091011121314"

pub const other_hash =
  #"02030405060708091011121314150203040506070809101112131415"

pub const beneficiary_keyhash =
  #"03040506070809101112131415160304050607080910111213141516"

pub const collector_keyhash =
  #"04050607080910111213141516170405060708091011121314151617"

pub fn beneficiary_address() {
  keyhash_address(beneficiary_keyhash)
}

pub fn collector_address() {
  keyhash_address(collector_keyhash)
}

pub fn own_address() {
  scripthash_address(own_hash)
}

pub fn other_address() {
  scripthash_address(other_hash)
}

pub fn keyhash_address(keyhash: ByteArray) {
  Address {
    payment_credential: VerificationKeyCredential(keyhash),
    stake_credential: None,
  }
}

pub fn scripthash_address(scripthash: ByteArray) {
  Address {
    payment_credential: ScriptCredential(scripthash),
    stake_credential: None,
  }
}
