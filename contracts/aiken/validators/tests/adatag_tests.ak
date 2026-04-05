use adatag
use aiken/crypto.{ScriptHash}
use aiken/interval
use aiken/primitive/bytearray
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use ilap/adatag/tests/fixture.{minting_policy, stateholder_hash, timedeposit_address, timedeposit_hash, token_policy} as tag_fixture
use ilap/adatag/types.{
  AdatagAdded, MintParams, MintRedeemer, Minting, State, TimeDepositDatum,
}
use ilap/integri_tree/tests/fixture as tree_fixture
use tests/validator_test_utils.{before_deactivation_interval, mk_valid_mint_value, mock_mint_params}


/// Property test for proper minting of Adatag state token
/// Maxing tree size to 2^28 - 1 = 268435455
/// 
test test_adatag_proper_minting_2_28() {
  let (adatag, tree_size, root, root1, nun, nun1, proof, _) =
    tree_fixture.generate_proof(64)

    let rdmr = Minting(MintRedeemer { update_val: nun, append_val: nun1, proof })


      let tx =
    Transaction {
      ..transaction.placeholder,
      mint: mk_valid_mint_value(adatag),
      /// Minting policy just validating the adahandle's input and only if deactivation time has not passed
      ///
      //inputs: [input],
      //outputs: [state_output, timedeposit_output, adahandle_output],
      validity_range: before_deactivation_interval,
    }

  adatag.adatag_minting.mint(
    params: mock_mint_params,
    rdmr: rdmr,
    policy_id: minting_policy,
    self: tx,
  )?
}

test test_adatag_minting_2_32() {
  let adatag = "ilaprocks"
  let (adatag, tree_size, root, root1, nun, nun1, proof, proof1) =
    tree_fixture.generate_valid_proof(adatag, 64)

  let day = 86400000
  let curr_time = day
  let three_hours = 10800000
  let twenty_days = curr_time + 20 * day

  let os =
    State {
      operation_count: 1,
      operation: AdatagAdded,
      size: "131071",
      // 131072
      adatag: "idk",
      minting_policy,
      root_hash: root,
    }
  //trace @"Treesize: ": tree_size
  //trace @"Loong proof: ": proof
  let ns =
    State {
      operation_count: 2,
      operation: AdatagAdded,
      size: tree_size,
      adatag,
      minting_policy,
      root_hash: root1,
    }

  let mv =
    assets.zero
      |> assets.add(minting_policy, adatag, 1)
  let vv =
    assets.zero
      |> assets.add("", "", 100)
      |> assets.add(token_policy, bytearray.take(adatag, 1), 1)
  let output =
    Output {
      address: tag_fixture.state_holder_address,
      value: vv,
      datum: InlineDatum(os),
      reference_script: None,
    }

  let state_output =
    Output {
      address: tag_fixture.state_holder_address,
      value: vv,
      datum: InlineDatum(ns),
      reference_script: None,
    }

  let tv =
    assets.zero
      |> assets.add("", "", 218)

  let timedeposit_datum =
    TimeDepositDatum { beneficiary: "", deadline: twenty_days }
  let timedeposit_output =
    Output {
      address: timedeposit_address,
      value: tv,
      datum: InlineDatum(timedeposit_datum),
      reference_script: None,
    }

  let ahs =
    #"4876543210987654321098765432109876543210987654321098765432109876543210"
  let av =
    assets.zero
      |> assets.add("", "", 100)
      |> assets.add(ahs, adatag, 1)
  let adahandle_output =
    Output {
      address: tag_fixture.scripthash_address(
        #"3876543210987654321098765432109876543210987654321098765432109876543210",
      ),
      value: av,
      datum: InlineDatum(Void),
      reference_script: None,
    }

  let utxo =
    OutputReference {
      transaction_id: transaction.placeholder.id,
      output_index: 0,
    }

  let input = Input { output_reference: utxo, output }

  let iv = interval.between(curr_time - three_hours, curr_time + three_hours)

  let tx =
    Transaction {
      ..transaction.placeholder,
      mint: mv,
      //extra_signatories: [tag_fixture.other_hash],
      inputs: [input],
      outputs: [state_output, timedeposit_output, adahandle_output],
      validity_range: iv,
    }

  let rdmr = Minting(MintRedeemer { update_val: nun, append_val: nun, proof: proof })

  let params =
    MintParams {
      token_policy,
      stateholder_hash: stateholder_hash,
      timedeposit_hash: timedeposit_hash,
      deposit_base: 1750,
      // Preferably 3-6 months after bootstrap
      // 1 day after start
      deactivation_time: 86400000,
      // 20 days - 3hrs in ms
      // Preferably 20 or N days - 3 hours (3 hours should be the max upper bound in the validity range.)
      locking_days: 1717200000,
      // 20 * 86400000 - 3 * 3600000 -> 1728000000 - 10800000,
      adahandle: #"6876543210987654321098765432109876543210987654321098765432109876543210",
    }

  adatag.adatag_minting.mint(
    params: params,
    rdmr: rdmr,
    policy_id: minting_policy,
    self: tx,
  )?
}
