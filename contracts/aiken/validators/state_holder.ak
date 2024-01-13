use aiken/bytearray
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, Redeemer, ScriptContext, Spend,
  Transaction, TransactionId, find_input,
}
use aiken/transaction/value.{add}
use ilap/adatag/tests/fixture.{other_hash, own_address, own_hash}
use ilap/adatag/types.{AdatagAdded, ScriptHash, TreeState}
use ilap/adatag/utils.{correct_nft_amount, has_only_token, parse_int}

//use aiken/cbor
//use ilap/integri_tree/hash.{combine_three_hashes, hash_val}
//use ilap/integri_tree/types as tree
//use ilap/integri_tree/proofs.{empty_hash}

/// This is a simple parameterized validator designed for handling (by UtxOs) control
/// NFT tokens and the state of a tree used for minting and burning adatags.
///
/// For example, in a UTXO that contains the validator address, an auth token in the
/// value field, and the state of the relevant tree in the datum field.
///
/// Therefore, this validator's primary function is to transfer (spend) the control 
/// NFT to its own address only when a minting policy is present in the transaction
/// and the other required conditions are met.
///
/// Assuming proper bootstrap, meaning:
/// There are only 26 auth tokens sitting on 26 StateHolder's address, with 26 initial datums
/// i.e. 26 individual EUTxOs holding the state of the reelvant adatag trees.
///
/// E.g. 
/// Validator Address, Curr symbol, token name, amount, datum
/// validator_address, auth_token_polid,  "a",        1,      initial_state_a
/// ...
/// validator_address, auth_token_polid,  "z",        1,      initial_state_z
///
/// >Note: initial_state_a's proof i.e. root hash of ProofTree {Val { xi: 1, xa: "`", xb: "b"}, left: empty_hash, right: empty_hash}
///
validator(auth_token: ScriptHash) {
  fn state_holder(
    old_state: TreeState,
    _r: Redeemer,
    ctx: ScriptContext,
  ) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    expect Spend(own_ref) = purpose

    let Transaction { inputs, outputs, mint, .. } = transaction

    // Get the own address to find the outputs.
    expect Some(own_input) = find_input(inputs, own_ref)

    let Output { address: input_address, .. } = own_input.output

    // Condition(0): Only one output is epxected/allowed
    expect [own_output] =
      list.filter(
        outputs,
        fn(output: Output) { output.address == input_address },
      )

    // Condition(1): Only a valid inline datum is allowed.
    expect InlineDatum(new_state) = own_output.datum
    expect new_state: TreeState = new_state

    // Condition(2): The MintValue's policy == new_state.minting_policy
    // Contidion(3): The minted asset class (symbol, token name, quantity) ==
    // new_state.minting_policy, new_state.adatag and +/- 1 (+ when AdatagAdded otherwise -)
    let minting = new_state.operation == AdatagAdded

    expect
      correct_nft_amount(
        minting: minting,
        mint: mint,
        symbol: new_state.minting_policy,
        token_name: new_state.adatag,
      )

    // Condition(4): the own output's value must only contain a valid
    // authorization token (as parameter of the validator). It's valid when the adatag's 
    // 1st letter == authorization token.
    // e.g. adatag == "ilap" then auth_token's token name must be "i"
    let output_value = own_output.value
    expect
      has_only_token(
        output_value,
        auth_token,
        bytearray.take(new_state.adatag, 1),
        1,
      )
    // Condition(5): No other than AdaSymbol and the auth token's policies (currency symbols) 
    // are alowed in the output.
    expect list.length(value.policies(output_value)) == 2

    let old_size = parse_int(old_state.size)
    let new_size = parse_int(new_state.size)

    // Tree state's saniti checks
    and {
      // Condition(6): the operation count is always increased
      new_state.operation_count == old_state.operation_count + 1,
      // Condition(7): new tree size cannot be less than 0
      new_size >= 0,
      // Condition(8): new tree size must be -/+ 1 as the old tree size. 
      new_size == old_size + if minting {
        1
      } else {
        -1
      },
      // mint_amount in Condition(3)
      // Condition(9): the minting policy must be same in the old and new state 
      // and in mint value (Condition(2))
      old_state.minting_policy == new_state.minting_policy,
    }
  }
}

test test_state_holder() {
  let mp = own_hash
  let auth_token = other_hash
  let adatag = "ilap"

  let os =
    TreeState {
      operation_count: 1,
      operation: AdatagAdded,
      size: "10",
      adatag: "idk",
      minting_policy: mp,
      root_hash: "",
    }

  let ns =
    TreeState {
      operation_count: 2,
      operation: AdatagAdded,
      size: "11",
      adatag: "ilap",
      minting_policy: mp,
      root_hash: "",
    }

  let mv =
    value.zero()
      |> add(mp, adatag, 1)
  let vv =
    value.zero()
      |> add("", "", 100)
      |> add(auth_token, bytearray.take(adatag, 1), 1)
  let output =
    Output {
      address: own_address(),
      value: vv,
      datum: InlineDatum(os),
      reference_script: None,
    }

  let state_output =
    Output {
      address: own_address(),
      value: vv,
      datum: InlineDatum(ns),
      reference_script: None,
    }

  let utxo =
    OutputReference { transaction_id: TransactionId(""), output_index: 0 }

  let input = Input { output_reference: utxo, output }

  let tx =
    Transaction {
      ..transaction.placeholder(),
      mint: value.to_minted_value(mv),
      extra_signatories: [other_hash],
      inputs: [input],
      outputs: [state_output],
    }
  let ctx = ScriptContext { purpose: Spend(utxo), transaction: tx }

  state_holder(Void, auth_token: auth_token, old_state: os, ctx: ctx)
}
//test test_tree_state() {
//   let ns =
//    TreeState {
//      operation_count: 1110,
//      operation: AdatagAdded,
//      size: "0",
//      adatag: "",
//      minting_policy: "9228a8330752bfb2ad4962eb9ebe8eabf397d7174e41cb525363c8d6",
//      root_hash: "",
//    }

//  let ts = "d8799f00d87a8040410058354500340003210002230050000722008700420050000491000009950002210213011004890002007500360000082502060513051500581c9228a8330752bfb2ad4962eb9ebe8eabf397d7174e41cb525363c8d6ff"

//cbor.diagnostic(ns) == @"a"
//cbor.serialise(ns) == ""
//  let cs = combine_three_hashes(hash_val(tree.Val {xi: "0", xa: "y", xb: "{"}), empty_hash, empty_hash)
//  cs == ""
//True
//}
