//// Standard utilities for Adatag's validators and minting policies retrieving the:
//// 0. Input and output of a script
//// 1. Input datum, value
//// 2. Output datum, value
//// 3. token and amount of a value (MintedValue, Value)
//// 4. Has signed by and address or credential etc.

use aiken/builtin.{append_string}
use aiken/cbor
use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/interval.{Finite}
use aiken/primitive/bytearray
use cardano/address.{Script, VerificationKey}
use cardano/assets.{AssetName, PolicyId, Value, add, quantity_of, tokens, zero}
use cardano/transaction.{
  Datum, InlineDatum, Input, Output, OutputReference, ValidityRange,
}
use ilap/adatag/tests/fixture.{other_address, own_address, own_hash}

pub fn debug_message(message: String, data: Data) -> String {
  let prefix = builtin.append_string(message, @": ")
  append_string(prefix, cbor.diagnostic(data))
}

pub fn has_only_token(
  val: Value,
  policy_id: PolicyId,
  token_name: ByteArray,
  token_quantity: Int,
) -> Bool {
  expect [Pair(tn, quantity)] =
    val
      |> tokens(policy_id)
      |> dict.to_pairs()

  tn == token_name && quantity == token_quantity
}

pub fn correct_nft_amount(
  minting: Bool,
  mint: Value,
  symbol: ScriptHash,
  token_name: AssetName,
) {
  let mint_amount =
    if minting {
      1
    } else {
      -1
    }
  // TODO(ilap): check minted value
  let mint_value = mint
  has_only_token(mint_value, symbol, token_name, mint_amount)
}

pub fn find_script_inputs(
  inputs: List<Input>,
  script_hash: ScriptHash,
) -> List<Input> {
  inputs
    |> list.filter(
        fn(input) {
          when input.output.address.payment_credential is {
            Script(addr_script_hash) -> script_hash == addr_script_hash
            _ -> False
          }
        },
      )
}

pub fn output_has_token(
  outputs: List<Output>,
  policy_id: PolicyId,
  asset_name: AssetName,
) -> Bool {
  let result =
    outputs
      |> list.filter_map(
          fn(output) {
            let v = output.value

            if quantity_of(v, policy_id, asset_name) > 0 {
              Some(True)
            } else {
              None
            }
          },
        )
      |> list.head
  //|> Some
  when result is {
    None -> False
    Some(_) -> True
  }
}

test test_output_has_token() {
  let vv =
    zero
      |> add(own_hash, "ilap", 1)
      |> add("", "", 100)
  let output =
    Output {
      address: other_address(),
      value: vv,
      datum: InlineDatum(Void),
      reference_script: None,
    }
  output_has_token([output], own_hash, "ilap")
}

pub fn find_script_input_datum(
  inputs: List<Input>,
  script_hash: ScriptHash,
) -> List<Datum> {
  inputs
    |> list.filter_map(
        fn(input) {
          let output = input.output

          when input.output.address.payment_credential is {
            Script(addr_script_hash) ->
              if script_hash == addr_script_hash {
                Some(output.datum)
              } else {
                None
              }
            VerificationKey(_) -> None
          }
        },
      )
}

pub fn signed_by(tx_signatures: List<ScriptHash>, beneficiary: ByteArray) {
  list.has(tx_signatures, beneficiary)?
}

pub fn time_elapsed(range: ValidityRange, time: Int) -> Bool {
  let from = interval.after(time)

  expect Finite(lower_bound) = range.lower_bound.bound_type
  interval.contains(from, lower_bound)
}

test test_find_script_input_datum() {
  let output =
    Output {
      address: own_address(),
      value: zero,
      datum: InlineDatum(Void),
      reference_script: None,
    }

  let utxo = OutputReference { transaction_id: "", output_index: 0 }

  let input = Input { output_reference: utxo, output }

  expect [datum] =
    find_script_input_datum(inputs: [input], script_hash: own_hash)

  cbor.diagnostic(datum) != @"a"
}

pub fn is_valid_tag(tag: ByteArray) -> Bool {
  let n = bytearray.length(tag)

  // must have valid length
  when n > 0 && n <= 16 is {
    True -> {
      let fst = builtin.index_bytearray(tag, 0)
      let lst = builtin.index_bytearray(tag, n - 1)

      is_lowercase(fst) && // 1st char must be letter
      is_lowercase_or_digit(lst) && // Last char must be letters or digits
      has_only_valid_chars(tag)
    }
    // otherwise only allowed chars.
    _ -> False
  }
}

fn is_lowercase(ch: Int) -> Bool {
  ch >= 97 && ch <= 122
}

fn is_digit(ch: Int) -> Bool {
  ch >= 48 && ch <= 57
}

fn is_lowercase_or_digit(ch: Int) -> Bool {
  is_lowercase(ch) || is_digit(ch)
}

fn is_hyphen(ch: Int) -> Bool {
  ch == 95
}

fn is_underscore(ch: Int) -> Bool {
  ch == 45
}

fn is_dot(ch: Int) -> Bool {
  ch == 46
}

fn is_valid_char(ch: Int) -> Bool {
  is_lowercase_or_digit(ch) || is_hyphen(ch) || is_underscore(ch) || is_dot(ch)
}

fn has_only_valid_chars(tag: ByteArray) -> Bool {
  bytearray.foldl(
    tag,
    True,
    fn(byte, is_valid) { is_valid_char(byte) && is_valid },
  )
}

// aiken check -e -m "test_is_valid_tag"
test test_is_valid_tag() {
  !is_valid_tag("") && !is_valid_tag("abcdefg0123456789") && !is_valid_tag(
    "0ilap",
  ) && !is_valid_tag("-ilap") && !is_valid_tag("_ilap") && !is_valid_tag(
    "ilap-",
  ) && !is_valid_tag("ilap_") && is_valid_tag("ilap") && is_valid_tag("ilap") && is_valid_tag(
    "ilap0",
  ) && is_valid_tag("i-lap-0") && !is_valid_tag("Alap") && !is_valid_tag(
    "alapkaA",
  )
}

test test_is_valid_tag_good_shortest() {
  is_valid_tag("a")
}

test test_is_valid_tag_good_6() {
  is_valid_tag("abcde8")
}

test test_is_valid_tag_good_longest() {
  is_valid_tag("abcdef0123456789")
}

test test_is_valid_tag_quick_fail() {
  // Very quick fail as the last char is checked early
  !is_valid_tag("abcdefg0123456!")
}

test test_is_valid_tag_slow_fail() {
  !is_valid_tag("abcdefg012345!7")
}

/// A very simple string to unsigned integer parser.
/// It fails when the string contains any non digits
/// or starts with leading zeros (expect "0" string)
pub fn parse_int(string: ByteArray) -> Int {
  let fst = builtin.index_bytearray(string, 0)
  let len = bytearray.length(string)

  if fst == 48 {
    if len == 1 {
      0
    } else {
      // Leading zeros are not allowed
      fail
    }
  } else {
    bytearray.foldl(
      string,
      0,
      fn(byte, acc) {
        // Only digits are allowed
        expect byte >= 48 && byte <= 57
        let num = byte - 48
        acc * 10 + num
      },
    )
  }
}

test test_parse_ints() {
  // parse_int("12345678901") == 12345678901 && 
  // parse_int("0") == 0 && 
  parse_int("100000000000000000000000000000000000000000000") == 100000000000000000000000000000000000000000000
}
