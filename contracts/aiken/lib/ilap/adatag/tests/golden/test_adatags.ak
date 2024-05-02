use aiken/list
use ilap/adatag/tests/golden/data/invalid_tags.{get_invalid_adatags}
use ilap/adatag/tests/golden/data/valid_tags.{get_valid_adatags}
use ilap/adatag/utils.{is_valid_tag}

test test_valid_adatags_golden() {
  let adatags = get_valid_adatags()
  list.all(adatags, fn(adatag) { is_valid_tag(adatag) })
}

test test_invalid_adatags_golden() {
  let adatags = get_invalid_adatags()
  list.all(adatags, fn(adatag) { !is_valid_tag(adatag) })
}
