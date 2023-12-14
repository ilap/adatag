use aiken/dict
use aiken/hash// .{Blake2b_224, Hash}
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Output, Redeemer, ScriptContext, Spend, Transaction,
  find_input,
}
use aiken/transaction/credential.{Script}
use aiken/transaction/value.{quantity_of}

use ilap/labeled_tree/types.{Hash}

// type Hash =
//  Hash<Blake2b_224, Script>

type ScriptHash = hash.Hash<hash.Blake2b_224, Script>

// TODO: type TreeState {
//   InitialState
//  AdatagAdded
//  AdatagRemoved
//}

type StateHolderDatum {
  count: Int,
  adatag: ByteArray,
  // TODO: tree_state: TreeState,
  tree_size: Int,
  tree_proof: Hash,
  minting_policy: ScriptHash,
}

validator {
  fn state_holder(
    _datum: StateHolderDatum,
    _r: Redeemer,
    ctx: ScriptContext,
  ) -> Bool {
    let ScriptContext { transaction, purpose } = ctx

    expect Spend(own_ref) = purpose
    expect Some(own_input) = find_input(transaction.inputs, own_ref)

    expect InlineDatum(maybe_old_datum) = own_input.output.datum
    expect old_datum: StateHolderDatum = maybe_old_datum

    let script_address = own_input.output.address

    // TODO: let token_name = old_datum.adatag
    expect Some(output) =
      transaction.outputs
        |> list.filter(fn(output) { output.address == script_address })
        |> list.head()
    expect InlineDatum(maybe_new_datum) = output.datum
    expect new_datum: StateHolderDatum = maybe_new_datum

    and {
      new_datum.count == old_datum.count + 1,
      new_datum.tree_size >= 0,
      old_datum.minting_policy == new_datum.minting_policy,
    }
  }
}

fn output_has_authorizing_nft(
  token_name: ByteArray,
  output: Output,
  policy_id: ScriptHash,
) {
  1 == quantity_of(output.value, policy_id, token_name)
}

fn has_mint(
  policy_id: ScriptHash,
  token_name: ByteArray,
  transaction: Transaction,
) {
  let Transaction { mint, .. } = transaction

  // 
  expect [(asset_name, amount)] =
    mint
      |> value.from_minted_value
      |> value.tokens(policy_id)
      |> dict.to_list()

  amount == 1 && asset_name == token_name
}
