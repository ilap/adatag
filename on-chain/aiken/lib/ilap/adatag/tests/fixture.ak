// This file creates some fixtures we can use for testing

use aiken/dict
use aiken/interval.{Interval, IntervalBound, NegativeInfinity, PositiveInfinity}
use aiken/transaction.{
  Input, NoDatum, Output, OutputReference, Transaction, TransactionId,
}
use aiken/transaction/credential.{
  Address, ScriptCredential, VerificationKeyCredential,
}
use aiken/transaction/value

pub const own_hash = #"01020304050607080910111213140102030405060708091011121314"

pub const other_hash = #"02030405060708091011121314150203040506070809101112131415"

pub const beneficiary_keyhash = #"03040506070809101112131415160304050607080910111213141516"

pub const collector_keyhash = #"04050607080910111213141516170405060708091011121314151617"

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

// Required params: inputs, outputs, mintedvalue, 
pub fn tx(reference_inputs: List<Input>, outputs: List<Output>) -> Transaction {
  let tx =
    Transaction {
      inputs: [
        Input {
          output_reference: OutputReference {
            transaction_id: TransactionId { hash: #"00" },
            output_index: 0,
          },
          output: Output {
            address: scripthash_address(#"010203"),
            value: value.from_asset(#"010203", #"000643b0427564", 1),
            datum: NoDatum,
            reference_script: None,
          },
        },
      ],
      reference_inputs,
      outputs,
      fee: value.zero(),
      mint: value.to_minted_value(value.zero()),
      certificates: [],
      withdrawals: dict.new(),
      validity_range: Interval {
        lower_bound: IntervalBound {
          bound_type: NegativeInfinity,
          is_inclusive: True,
        },
        upper_bound: IntervalBound {
          bound_type: PositiveInfinity,
          is_inclusive: True,
        },
      },
      extra_signatories: [#"00010203040506"],
      redeemers: dict.new(),
      datums: dict.new(),
      id: TransactionId { hash: #"" },
    }
  tx
}
