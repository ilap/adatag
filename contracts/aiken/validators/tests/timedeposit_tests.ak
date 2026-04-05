use aiken/interval
use ilap/adatag/tests/fixture.{beneficiary_keyhash, collector_keyhash}
use ilap/adatag/types.{Collect, Redeem}
use ilap/adatag/utils.{time_elapsed}
use tests/validator_test_utils.{
  invalid_datum, mk_timedeposit_tx as mk_tx, timedeposit_params,
  valid_timedeposit_datum,
}
use timedeposit

// ============================================================================
// Helpers for tests
// ============================================================================

/// ``` markdown
/// | Case    | Datum            | Time condition                 | Signer      | Result |
/// | ------- | ---------------- | ------------------------------ | ----------- | ------ |
/// | Collect | ❌ invalid / none | irrelevant                     | collector   | ✅ PASS |
/// | Collect | ❌ invalid / none | irrelevant                     | ❌           | ❌ FAIL |
/// | Collect | ✅ valid          | collection time passed         | collector   | ✅ PASS |
/// | Collect | ✅ valid          | collection time **not** passed | collector   | ❌ FAIL |
/// | Collect | ✅ valid          | collection time passed         | ❌           | ❌ FAIL |
/// | Redeem  | ✅ valid          | deadline passed                | beneficiary | ✅ PASS |
/// | Redeem  | ❌ invalid / none | passed                         | beneficiary | ❌ FAIL |
/// | Redeem  | ✅ valid          | deadline **not** passed        | beneficiary | ❌ FAIL |
/// | Redeem  | ✅ valid          | deadline passed                | ❌           | ❌ FAIL |
/// ```
const coll_time = 100

// -----------------------------
// Collect Tests
// -----------------------------

test timedeposit_collect_invalid_datum_pass_anytime() {
  timedeposit.timedeposit.spend(
    timedeposit_params(coll_time),
    invalid_datum(),
    Collect,
    Void,
    mk_tx(True, 0, coll_time - 1, [collector_keyhash]),
  )
}

// 1️⃣ invalid datum, collector signs -> PASS
test timedeposit_collect_pass_invalid_datum_collector() {
  timedeposit.timedeposit.spend(
    timedeposit_params(coll_time),
    invalid_datum(),
    Collect,
    Void,
    mk_tx(True, 0, 9999, [collector_keyhash]),
  )
}

// 2️⃣ no datum, collector signs -> PASS
test timedeposit_collect_pass_no_datum_collector() {
  timedeposit.timedeposit.spend(
    timedeposit_params(coll_time),
    None,
    Collect,
    Void,
    mk_tx(True, 0, 9999, [collector_keyhash]),
  )
}

// 3️⃣ invalid datum, missing collector -> FAIL
test timedeposit_collect_fail_invalid_datum_no_signer() fail {
  expect
    fail timedeposit.timedeposit.spend(
      timedeposit_params(coll_time),
      invalid_datum(),
      Collect,
      Void,
      mk_tx(True, 0, 9999, []),
    )
}

// 4️⃣ valid datum, collection time passed, collector signs -> PASS
test timedeposit_collect_pass_valid_time_passed() {
  timedeposit.timedeposit.spend(
    timedeposit_params(coll_time),
    valid_timedeposit_datum(beneficiary_keyhash, 200),
    Collect,
    Void,
    mk_tx(True, coll_time + 1, 9999, [collector_keyhash]),
  )
}

// 5️⃣ valid datum, collection time NOT passed, collector signs -> FAIL
test timedeposit_collect_fail_valid_time_not_passed() fail {
  expect
    fail timedeposit.timedeposit.spend(
      timedeposit_params(coll_time),
      valid_timedeposit_datum(beneficiary_keyhash, 200),
      Collect,
      Void,
      mk_tx(True, 0, 50, [collector_keyhash]),
    )
}

// 6️⃣ valid datum, collection time passed, missing collector -> FAIL
test timedeposit_collect_fail_valid_time_passed_no_signer() fail {
  expect
    fail timedeposit.timedeposit.spend(
      timedeposit_params(coll_time),
      valid_timedeposit_datum(beneficiary_keyhash, 200),
      Collect,
      Void,
      mk_tx(True, 100, 9999, []),
    )
}

// -----------------------------
// Redeem Tests
// -----------------------------

// 7️⃣ valid datum, deadline passed, beneficiary signs -> PASS
test timedeposit_redeem_pass_deadline_passed() {
  timedeposit.timedeposit.spend(
    timedeposit_params(coll_time),
    valid_timedeposit_datum(beneficiary_keyhash, coll_time),
    Redeem,
    Void,
    mk_tx(True, coll_time + 1, 9999, [beneficiary_keyhash]),
  )
}

// 8️⃣ invalid datum, beneficiary signs -> FAIL
test timedeposit_redeem_fail_invalid_datum() fail {
  expect
    fail timedeposit.timedeposit.spend(
      timedeposit_params(coll_time),
      invalid_datum(),
      Redeem,
      Void,
      mk_tx(True, coll_time + 1, 9999, [beneficiary_keyhash]),
    )
}

// 9️⃣ valid datum, deadline NOT passed, beneficiary signs -> FAIL
test timedeposit_redeem_fail_deadline_not_passed() fail {
  expect
    fail timedeposit.timedeposit.spend(
      timedeposit_params(coll_time),
      valid_timedeposit_datum(beneficiary_keyhash, coll_time),
      Redeem,
      Void,
      mk_tx(True, 50, 99, [beneficiary_keyhash]),
    )
}

// 1️⃣0️⃣ valid datum, deadline passed, missing beneficiary -> FAIL
test timedeposit_redeem_fail_deadline_passed_no_signer() fail {
  expect
    fail timedeposit.timedeposit.spend(
      timedeposit_params(coll_time),
      valid_timedeposit_datum(beneficiary_keyhash, coll_time),
      Redeem,
      Void,
      mk_tx(True, coll_time + 1, 9999, []),
    )
}

// Tests for time_elapsed function with Plutus V3 semantics
test timedeposit_elapsed_plutus_v3_boundary_and_infinity_semantics() {
  let unlock_at = 1_000
  let from = unlock_at
  let to = unlock_at + 10_000

  // Full open, i.e. lower bound is inclusive
  expect time_elapsed(interval.entirely_between(from, to), unlock_at)
  // Lover bound is +Infinity for empty interval
  expect time_elapsed(interval.empty, unlock_at)
  // Semi open scenarios, i.e. lower bound is exclusive
  expect False = time_elapsed(interval.between(from, to), unlock_at)
  expect False = time_elapsed(interval.everything, unlock_at)
}
