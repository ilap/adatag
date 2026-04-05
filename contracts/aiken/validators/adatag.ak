use aiken/crypto.{ScriptHash}
use aiken/interval.{Finite}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, quantity_of}
use cardano/transaction.{
  InlineDatum, Input, Output, Transaction, ValidityRange, find_script_outputs,
}
use ilap/adatag/types.{
  Burning, MintAction, MintParams, MintRedeemer, Minting, PosixTime, State,
  TimeDepositDatum,
}
use ilap/adatag/utils.{
  find_script_inputs, is_valid_tag, output_has_token, time_elapsed,
}
// TODO: use ilap/integri_tree/delete_proofs.{delete_check}
use ilap/integri_tree/proofs.{append_check}

validator adatag_minting(params: MintParams) {
  mint(rdmr: MintAction, policy_id: PolicyId, self: Transaction) {
    let Transaction { inputs, outputs, mint, validity_range, .. } = self

    // Retrieve the state
    // Only one input is allowed
    trace @"find_script_inputs"
    expect [validator_input] =
      find_script_inputs(inputs: inputs, script_hash: params.stateholder_hash)
    trace @"expect InlineDatum(old_state) = validator_input.output.datum"
    expect InlineDatum(old_state) = validator_input.output.datum
    trace @"expect old_state: State = old_state"
    expect old_state: State = old_state

    // Retrieve the state'
    // only one output is allowed
    trace @"find_script_outputs"
    expect [validator_output] =
      find_script_outputs(outputs, params.stateholder_hash)
    trace @"expect InlineDatum(new_state) = validator_output.datum"
    expect InlineDatum(new_state) = validator_output.datum
    trace @"New state"
    expect new_state: State = new_state

    trace @"mint_amount"
    // Retrive the mint amount, keep in mind state holder validator did all the sanity check.
    let mint_amount = quantity_of(mint, policy_id, new_state.adatag)

    trace @"complex validation"
    and {
      is_valid_tag(new_state.adatag)?,
      check_timedeposit(new_state.adatag, outputs, params, validity_range)?,
      when rdmr is {
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
            )?,
          }
        Burning(_redeemer) -> and {
            mint_amount == -1,
            // TODO: implement burning
            False,
          }
      },
    }
    // FIXME: Implement
  }

  else(_) {
    fail
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
      params.timedeposit_hash,
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
