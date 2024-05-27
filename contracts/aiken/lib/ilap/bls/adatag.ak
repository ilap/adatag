use bls/bls.{aggregate_signatures, sign, skToPk, verify, verify_aggregate}
use ilap/integri_tree/hash.{empty_hash, hash_val}
use ilap/integri_tree/types.{Val}

const sk1 = #"136eb45dcd7e47d321da0e11818d009f30d4ebe7df874c034e95fc3ea3fe33a2"

const sk2 = #"4233a95be450de785e3cf253f59bb1ba6d9e0751243a1d8595b1eac99afb984b"

const sk3 = #"042f844662445efb31d6666408f52394584466244809c36701b0ff44ebc36dfa"

const sk4 = #"5efb31d66664088446624438ace5c09c36701b08446624438ace5cb38ace5ce5"

/// Proof of concept using BLS instead of IntegriTree
/// 
test test_bls_validation2() {
  let val1 = Val { xi: "0", xa: "a", xb: "z" }
  let val2 = Val { xi: "0", xa: "a", xb: "b" }
  let val3 = Val { xi: "1", xa: "b", xb: "z" }

  let nm_valh = hash_val(val1)
  let m_valh1 = hash_val(val2)
  let m_valh2 = hash_val(val3)

  let old_sign = sign(sk2, nm_valh)
  let new_sign1 = sign(sk3, m_valh1)
  let new_sign2 = sign(sk4, m_valh2)

  let previous_state = sign(sk1, empty_hash)

  // Old state is stored on chain
  let old_state = aggregate_signatures([previous_state, old_sign])
  let new_state = aggregate_signatures([previous_state, new_sign1, new_sign2])

  let aggr_sign = aggregate_signatures([new_sign1, new_sign2])
  let old_pk = skToPk(sk2)
  let new_pk1 = skToPk(sk3)
  let new_pk2 = skToPk(sk4)
  // # 1. Verify non-member proof
  verify(old_pk, nm_valh, old_sign) && // # 2. Verify member proof created from non-member's Val
  new_pk1 != new_pk2 && verify_aggregate(
    [new_pk1, new_pk2],
    [m_valh1, m_valh2],
    aggr_sign,
  ) && // # 3. Verify old state
  old_state == aggregate_signatures([previous_state, old_sign]) && // # 4. Verify new state
  new_state == aggregate_signatures([previous_state, new_sign1, new_sign2])
}
