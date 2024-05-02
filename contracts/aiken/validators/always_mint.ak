/// A very simple one NFT minting policy for imitating adahandle in @adatag tests.
validator {
  /// No any check is necessary,
  fn mint(_r: Void, _ctx: Void) -> Bool {
    True
  }
}
