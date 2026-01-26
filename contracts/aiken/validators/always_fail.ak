/// Always fail validator as reference address for adatag validators
validator always_fail {
  spend(_d: Option<Void>, _r: Void, _out_ref: Void, _tx: Void) {
    // --keep-traces will keep the trace message, otherwise it will prune it.
    trace @"The Times 03/Jan/2009 Chancellor on brink of second bailout for banks"
    False
  }

  else(_) {
    fail @"unsupported purpose"
  }
}

/// It must always fail
test always_fail_test_must_fail() {
  !always_fail.spend(None, Void, Void, Void)
}
