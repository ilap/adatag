use aiken/bytearray
use aiken/interval.{Finite}
use aiken/time.{PosixTime}
use aiken/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, ScriptContext, Transaction,
  TransactionId, ValidityRange, find_script_outputs,
}
use aiken/transaction/value.{from_minted_value, quantity_of}
use ilap/adatag/tests/fixture as tag_fixture
use ilap/adatag/types.{
  AdatagAdded, Burning, MintAction, MintParams, MintRedeemer, Minting,
  ScriptHash, TimeDepositDatum, TreeState,
}
use ilap/adatag/utils.{
  find_script_inputs, is_valid_tag, output_has_token, time_elapsed,
}
use ilap/integri_tree/delete_proofs.{delete_check}
use ilap/integri_tree/proofs.{append_check}
use ilap/integri_tree/tests/fixture as tree_fixture

validator(params: MintParams) {
  fn adatag_minting(rdmr: MintAction, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    expect Mint(own_policy) = purpose

    let Transaction { inputs, outputs, mint, validity_range, .. } = transaction

    // Retrieve the state
    // Only one input is allowed
    expect [validator_input] =
      find_script_inputs(inputs: inputs, script_hash: params.state_holder)
    expect InlineDatum(old_state) = validator_input.output.datum
    expect old_state: TreeState = old_state

    // Retrieve the state'
    // only one output is allowed
    expect [validator_output] =
      find_script_outputs(outputs, params.state_holder)
    expect InlineDatum(new_state) = validator_output.datum
    expect new_state: TreeState = new_state

    // Retrive the mint amount, keep in mind state holder validator did all the sanity check.
    let mint_amount =
      quantity_of(from_minted_value(mint), own_policy, new_state.adatag)

    is_valid_tag(new_state.adatag) && check_timedeposit(
      new_state.adatag,
      outputs,
      params,
      validity_range,
    ) && when rdmr is {
      Minting(redeemer) -> and {
          mint_amount == 1,
          append_check(
            new_state.adatag,
            new_state.size,
            old_state.root_hash,
            new_state.root_hash,
            redeemer.update_val,
            redeemer.append_val,
            redeemer.proof,
          ),
        }
      Burning(redeemer) -> and {
          mint_amount == -1,
          delete_check(
            new_state.adatag,
            old_state.root_hash,
            new_state.root_hash,
            redeemer.update_val1,
            redeemer.update_val2,
            redeemer.delete_val,
            redeemer.proof,
          ),
        }
    }
  }
}

// Timedeposit check
// Condition(0): if the time lock deposit feature is not active anymore than pass, otherwise:
// Condition(1): if there is an adahandle (== adatag) in the ouptuts then pass, otherwise:
// Condition(2): if there is any valid timedeposit output (valid deadline, and valid deposit amount) then pass,
// otherwise fail
fn check_timedeposit(
  adatag: ByteArray,
  outputs: List<Output>,
  params: MintParams,
  range: ValidityRange,
) -> Bool {
  // Assuming short-circuit evaluation, meaning when the first True is found then it passes.
  or {
    time_elapsed(range, params.deactivation_time)?,
    output_has_token(outputs, params.adahandle, adatag)?,
    has_valid_timedeposit_output(
      params,
      range,
      outputs,
      params.timedeposit_validator,
      adatag,
    )?,
  }
}

fn has_valid_timedeposit_output(
  params: MintParams,
  range: ValidityRange,
  outputs: List<Output>,
  policy_id: ScriptHash,
  adatag: ByteArray,
) {
  // Expect only 1 timedeposit output
  expect [output] = find_script_outputs(outputs, policy_id)

  expect InlineDatum(timedeposit_datum) = output.datum
  expect timedeposit_datum: TimeDepositDatum = timedeposit_datum
  let deadline = timedeposit_datum.deadline

  let deposit = quantity_of(output.value, "", "")
  has_valid_deposit(params.deposit_base, deposit, bytearray.length(adatag))? && has_valid_deadline(
    params,
    range,
    deadline,
  )?
}

// The user must supply a proper validity range as the  [-Infinity, +Infinity] would fail.
// A valid range at tx creation time must contain an upper-bound (deadline > upper-bound + days-in-ms).
// while at claiming time a lower-bound (deadline > lower-bound).
// and valid deadline (~20 days in the future )
// the calculation should allows ~3 hours difference on the mainnet, assuming the upper bound is at max. 3 hours in the future.
// range   :--- [lb ----- ct -- 3hrs -- ub] +---------- 20 days - 3 hrs in ms ----------]  dl --------------------
// datum dl:              ct ----------------- ~ 20 days --------------------------------- ~deadline ----
fn has_valid_deadline(
  params: MintParams,
  range: ValidityRange,
  deadline: PosixTime,
) -> Bool {
  expect Finite(upper_bound) = range.upper_bound.bound_type
  let dl = upper_bound + params.locking_days

  dl <= deadline
}

fn has_valid_deposit(base: Int, deposit: Int, adatag_len: Int) -> Bool {
  when adatag_len is {
    // ~1750 ADA preferably
    1 -> deposit >= base
    //  ~875 ADA
    2 -> deposit >= base / 2
    //  ~437 ADA
    3 -> deposit >= base / 4
    //  ~218 ADA
    4 -> deposit >= base / 8
    //  ~109 ADA
    5 -> deposit >= base / 16
    //   ~54 ADA
    6 -> deposit >= base / 32
    //     15 ADA otherwise
    _ -> deposit >= 15
  }
}

test test_adatag_minting_2_20() {
  let (adatag, tree_size, root, root1, nun, nun1, proof, _) =
    tree_fixture.generate_proof(40)

  let mpv: ScriptHash =
    #"1876543210987654321098765432109876543210987654321098765432109876543210"
  let shv: ScriptHash = tag_fixture.own_hash
  let auth_token: ScriptHash = tag_fixture.other_hash
  let tdv: ScriptHash =
    #"9876543210987654321098765432109876543210987654321098765432109876543210"
  // 1 day in ms i.e. 86400 * 1000 = 86400000
  let day = 86400000
  let curr_time = day
  let three_hours = 10800000
  let twenty_days = curr_time + 20 * day

  let os =
    TreeState {
      operation_count: 1,
      operation: AdatagAdded,
      size: "131071",
      adatag: "idk",
      minting_policy: mpv,
      root_hash: root,
    }

  let ns =
    TreeState {
      operation_count: 2,
      operation: AdatagAdded,
      size: tree_size,
      adatag,
      minting_policy: mpv,
      root_hash: root1,
    }

  let mv =
    value.zero()
      |> value.add(mpv, adatag, 1)
  let vv =
    value.zero()
      |> value.add("", "", 100)
      |> value.add(auth_token, bytearray.take(adatag, 1), 1)
  let output =
    Output {
      address: tag_fixture.own_address(),
      value: vv,
      datum: InlineDatum(os),
      reference_script: None,
    }

  let state_output =
    Output {
      address: tag_fixture.own_address(),
      value: vv,
      datum: InlineDatum(ns),
      reference_script: None,
    }

  let tv =
    value.zero()
      |> value.add("", "", 218)

  let timedeposit_datum =
    TimeDepositDatum { beneficiary: "", deadline: twenty_days }
  let timedeposit_output =
    Output {
      address: tag_fixture.scripthash_address(tdv),
      value: tv,
      datum: InlineDatum(timedeposit_datum),
      reference_script: None,
    }

  let ahs =
    #"4876543210987654321098765432109876543210987654321098765432109876543210"
  let av =
    value.zero()
      |> value.add("", "", 100)
      |> value.add(ahs, adatag, 1)
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
    OutputReference { transaction_id: TransactionId(""), output_index: 0 }

  let input = Input { output_reference: utxo, output }

  let iv = interval.between(curr_time - three_hours, curr_time + three_hours)

  let tx =
    Transaction {
      ..transaction.placeholder(),
      mint: value.to_minted_value(mv),
      extra_signatories: [tag_fixture.other_hash],
      inputs: [input],
      outputs: [state_output, timedeposit_output, adahandle_output],
      validity_range: iv,
    }
  let ctx = ScriptContext { purpose: Mint(mpv), transaction: tx }

  let rdmr = Minting(MintRedeemer { update_val: nun, append_val: nun1, proof })

  let params =
    MintParams {
      auth_token,
      state_holder: shv,
      timedeposit_validator: tdv,
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

  // ahs,
  adatag_minting(params: params, rdmr: rdmr, ctx: ctx)
}

test test_append_check_2_24() {
  let (
    adatag,
    tree_size,
    old_root_hash,
    new_root_hash,
    update_val,
    append_val,
    proof,
    _,
  ) = tree_fixture.generate_proof(48)

  append_check(
    adatag,
    tree_size,
    old_root_hash,
    new_root_hash,
    update_val,
    append_val,
    proof,
  )
}
