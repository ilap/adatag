/// A very simple one NFT minting policy for imitating adahandle in @adatag tests.
/// > Note: No any check/test is necessary,
validator always_mint {
  mint(_r: Option<Void>, _pid: Void, _tx: Void) {
    True
  }

  else(_) {
    fail @"unsupported purpose"
  }
}
