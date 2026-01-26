use aiken/collection/list
use cardano/assets.{add, zero}
use ilap/adatag/utils.{has_only_token}

test test_has_only_token() {
  let v =
    zero
      |> add("", "", 1)
      |> add("a", "b", 1)
  has_only_token(v, "a", "b", 1) && list.length(assets.policies(v)) == 2
}
