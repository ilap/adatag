{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- {-# LANGUAGE StrictData #-}

module Contracts.TimeDeposit where

import qualified Data.ByteString.Char8 as BS8 (pack)
import Plutus.V1.Ledger.Interval (contains)
import Plutus.V2.Ledger.Api
  ( Datum (Datum),
    OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
    POSIXTime (..),
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInfo (txInfoValidRange),
    Validator,
    from,
    mkValidatorScript,
  )
import Plutus.V2.Ledger.Contexts (findDatum, txSignedBy)
import PlutusTx (FromData (fromBuiltinData), compile, unstableMakeIsData)
import PlutusTx.Builtins.Internal
import PlutusTx.Prelude (Bool, Maybe (..), traceError, traceIfFalse, ($), (&&))
import Utilities
  ( Network,
    printDataToJSON,
    validatorAddressBech32,
    wrapValidator,
    writeValidatorToFile,
  )
import Prelude (IO, Integer, String)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data TimeDepositDatum = TimeDepositDatum
  { ddBeneficiary :: PubKeyHash, -- beneficiary of the locked time deposit.
    ddDeadline :: POSIXTime, -- preferably 20 days, the minting policy will validate the deadline.

    -- | this prevents any adversaries using only 1 time-lock output and generating 26 adatag.
    ddAdatag :: BuiltinByteString
  }

unstableMakeIsData ''TimeDepositDatum

-- data TimeDepositParams = TimeDepositParams
--    { dpCollectorPkh          :: PubKeyHash -- Collecting donations (void datum is used) and unclaimed deposits (after a year or 2)
--    , dpCollectionTime        :: POSIXTime  -- it should be twice as deactivation time. 1-1.5yrs
--    , dpDeactivationTime      :: POSIXTime  -- When the time lock deposit feature is deactivated. Usially in 6-9 months of bootstrap
--    , dpAdaHandle             :: ValidatorHash
--    }
-- PlutusTx.makeLift ''TimeDepositParams

{-# INLINEABLE parseTimeDepositDatum #-}
parseTimeDepositDatum :: OutputDatum -> TxInfo -> Maybe TimeDepositDatum
parseTimeDepositDatum o info = case o of
  NoOutputDatum -> traceError "Found time deposit output but NoOutputDatum"
  OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
  OutputDatumHash dh -> do
    Datum d <- findDatum dh info
    PlutusTx.fromBuiltinData d

-- TimeDeposit validator for locking a certain amount of ADA for some time (20 days) at minting time
-- to prevent for buying a lot of the rare usernames and sell them on the market for high price.
{-# INLINEABLE mkTimeDepositValidator #-}
mkTimeDepositValidator :: TimeDepositDatum -> () -> ScriptContext -> Bool
mkTimeDepositValidator dat () ctx =
  traceIfFalse "beneficiary's signature missing" signedByBeneficiary
    && traceIfFalse "deadline not reached " deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ ddBeneficiary dat

    deadlineReached :: Bool
    deadlineReached = Plutus.V1.Ledger.Interval.contains (from $ ddDeadline dat) $ txInfoValidRange info

{-# INLINEABLE mkWrappedTimeDepositValidator #-}
mkWrappedTimeDepositValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedTimeDepositValidator = wrapValidator mkTimeDepositValidator

validator :: Validator
validator = mkValidatorScript $$(compile [||mkWrappedTimeDepositValidator||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./scripts/03-time-deposit.plutus" validator

timeDepositAddressBech32 :: Network -> String
timeDepositAddressBech32 network = validatorAddressBech32 network validator

printPosixTimeDepositDatumJSON :: PubKeyHash -> Integer -> String -> IO ()
printPosixTimeDepositDatumJSON pkh time adatag =
  printDataToJSON $
    TimeDepositDatum
      { ddBeneficiary = pkh,
        ddDeadline = POSIXTime time,
        ddAdatag = BuiltinByteString $ BS8.pack adatag
      }

{- See an exampe below, how to use it

Repl> import Plutus.V2.Ledger.Api
Repl> :set -XOverloadedStrings
Repl> printTimeDepositDatumJSON (PubKeyHash "alma") "2022-09-09T08:06:21.630747Z" "adatag"
{
    "constructor": 0,
    "fields": [
        {
            "bytes": "616c6d61"
        },
        {
            "int": 1662710781631
        },
        {
            "bytes": "616461746167"
        }
    ]
}
-}