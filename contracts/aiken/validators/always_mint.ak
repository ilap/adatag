use cardano/transaction.{OutputReference, Transaction}

/// A very simple one NFT minting policy for imitating adahandle in @adatag tests.
validator always_mint {
  /// No any check is necessary,
  mint(_r: Void, _own_ref: OutputReference, _transaction: Transaction) {
    True
  }

  else(_) {
    fail
  }
}
