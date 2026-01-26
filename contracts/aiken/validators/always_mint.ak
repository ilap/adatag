/// A very simple one NFT minting policy for imitating adahandle in @adatag tests.
validator always_mint {
  /// No any check is necessary,
  mint(_r: Option<Void>, _pid: Void, _tx: Void) {
    True
  }

  else(_) {
    fail @"unsupported purpose"
  }
}

/// It must always succeed
test always_mint_must_succeed() {
  always_mint.mint(None, Void, Void)
}
