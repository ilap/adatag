use aiken/list
use aiken/transaction/value.{add}
use ilap/adatag/utils.{has_only_token}

test test_has_only_token() {
  let v =
    value.zero()
      |> add("", "", 1)
      |> add("a", "b", 1)
  has_only_token(v, "a", "b", 1) && list.length(value.policies(v)) == 2
}
