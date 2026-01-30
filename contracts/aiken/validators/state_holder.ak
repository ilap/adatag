use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Redeemer, Transaction, find_input,
}
use ilap/adatag/types.{AdatagAdded, State}
use ilap/adatag/utils.{has_only_token, parse_uint, valid_nft_mint}

/// This is a simple parameterized validator designed for handling (by UtxOs) auth
/// NFT tokens and the state of an integri-tree used for minting and burning adatags.
///
/// For example, in a UTXO that contains the validator address, an auth token in the
/// value field, and the state of the relevant tree in the datum field.
///
/// Therefore, this validator's primary function is to transfer (spend) the auth 
/// NFT to its own address only when a minting policy is present in the transaction
/// and the other required conditions are met.
///
/// Assuming proper bootstrap, meaning:
/// There are only 26 auth tokens sitting on 26 StateHolder's addresses, with
/// 26 initial datums i.e. 26 individual EUTxOs holding the initial state of
/// the relevant adatag tree.
///
/// E.g. 
/// ```
/// Validator Address, Curr symbol, token name, amount, datum
/// validator_address, auth_token_polid,  "a",        1,      initial_state_a
/// ...
/// validator_address, auth_token_polid,  "z",        1,      initial_state_z
/// ```
/// >Note: initial_state_a's proof i.e. root hash of ProofTree {Val { xi: 1, xa: "`", xb: "b"}, left: empty_hash, right: empty_hash}
///
validator state_holder(auth_token: ScriptHash) {
  spend(
    datum: Option<State>,
    _redeemer: Redeemer,
    own_ref: OutputReference,
    transaction: Transaction,
  ) {
    let Transaction { inputs, outputs, mint, .. } = transaction

    // Find the script’s own input.
    expect Some(own_input) = find_input(inputs, own_ref)

    // Get the scripts own address from the input.
    let Output { address: input_address, .. } = own_input.output

    // Condition(0): Only one output is epxected/allowed
    expect [own_output] =
      list.filter(
        outputs,
        fn(output: Output) { output.address == input_address },
      )

    // Condition(1): Only a valid inline datum is allowed. 
    expect InlineDatum(new_state) = own_output.datum
    expect new_state: State = new_state

    // Condition(2): The MintValue's policy == new_state.minting_policy
    // Contidion(3): The minted asset class (symbol, token name, quantity) ==
    // new_state.minting_policy, new_state.adatag and +/- 1 (+ when AdatagAdded otherwise -)
    let minting = new_state.operation == AdatagAdded

    expect
      valid_nft_mint(
        minting: minting,
        // Assumption: no ADA policy is allowed by the ledger rules 
        // and only one policy is being minted/burned in the transaction
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
    expect list.length(assets.policies(output_value)) == 2

    expect Some(old_state) = datum

    let old_size = parse_uint(old_state.size)
    let new_size = parse_uint(new_state.size)

    // Tree state's sanity checks
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

  else(_) {
    fail @"unsupported purpose"
  }
}
