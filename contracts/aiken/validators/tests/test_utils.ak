use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId, Value, add}
use cardano/transaction.{Datum, Input, Output, OutputReference}

pub fn mk_outref(index: Int) -> OutputReference {
  OutputReference {
    transaction_id: transaction.placeholder.id,
    output_index: index,
  }
}

pub fn mk_input(output: Output, index: Int) -> Input {
  Input { output_reference: mk_outref(index: index), output }
}

pub fn mk_output(address: Address, value: Value, datum: Datum) -> Output {
  Output { address, value, datum, reference_script: None }
}

pub fn mk_asset(pid: PolicyId, asset_name: AssetName, amount: Int) -> Value {
  assets.zero |> add(pid, asset_name, amount)
}
