{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Contracts.TimeDeposit where

import qualified Data.ByteString.Char8 as BS8 (pack)
import Data.Maybe (fromJust)
import Plutus.V1.Ledger.Interval (contains)
import Plutus.V2.Ledger.Api
  ( Datum (Datum),
    OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
    POSIXTime (..),
    PubKeyHash (..),
    ScriptContext (scriptContextTxInfo),
    TxInfo (txInfoValidRange),
    Validator,
    from,
    mkValidatorScript,
    unsafeFromBuiltinData,
  )
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx (CompiledCode, FromData (fromBuiltinData), applyCode, compile, liftCode, makeLift, unstableMakeIsData)
import PlutusTx.Builtins.Internal
import PlutusTx.Prelude (Bool (..), Maybe (..), take, traceError, traceIfFalse, ($), (&&), (.))
import System.IO
import Text.Printf (printf)
import Utilities
  ( printDataToJSON,
    wrapValidator,
    writeDataToFile,
    writeValidatorToFile,
  )
import Utilities.Conversions
import Prelude (Show, String, show)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data TimeDepositDatum = -- |
                        TimeDepositDatum
  { -- |
    ddBeneficiary :: PubKeyHash, -- |

                        -- beneficiary of the locked time deposit.
    ddDeadline :: POSIXTime -- preferably ~20 days, the minting policy will validate the deadline at minting time.
    -- This prevents any adversaries generating 26 adatags at the same time but having only one time-lock output in the transaction.
    -- Minting policy handles this logic. It's not required for unlocking.
    -- TODO: ddAdatag :: BuiltinByteString - inf future version we would allow multiple minting in the same tx.
  }
  -- Note: the unit type () has a different signature than the {} and therefore different PLC data
  -- () = {"constructor": 1, "fields": [{"constructor": 0,"fields": []}]}"
  -- {} = {"constructor": 1, "fields": []}"
  | UnitDatum ()
  deriving (Prelude.Show)

unstableMakeIsData ''TimeDepositDatum

data TimeDepositParams = TimeDepositParams
  { dpCollector :: PubKeyHash, -- Collecting donations (void datum is used) and unclaimed deposits (after a year or two)
    dpCollectionTime :: POSIXTime -- The time the collector can collect the unclaimed time-lock deposits. It should be twice as deactivation time. ~1-2yrs
  }
  deriving (Prelude.Show)

PlutusTx.makeLift ''TimeDepositParams

-- Collector can collect any donation (having any non TimeDepositDatum) anytime or
-- after the collection time has passed with an UTxO having a valid TimeDepositDatum datum.
-- The beneficiary always can redeem after the deatline in the datum has passed
data TimeDepositRedeemer = Collect | Redeem

unstableMakeIsData ''TimeDepositRedeemer

-- Only inline datums are allowed.
{-# INLINEABLE parseTimeDepositDatum #-}
parseTimeDepositDatum :: OutputDatum -> Maybe TimeDepositDatum
parseTimeDepositDatum o = case o of
  NoOutputDatum -> traceError "Found time deposit output but NoOutputDatum"
  OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d -- Inline datum
  OutputDatumHash _ -> traceError "Found time deposit output but no Inline datum"

-- TimeDeposit validator for locking a certain amount of ADA for some time (20 days) at minting time
-- to prevent for buying a lot of the rare usernames and sell them on the market for high price.
{-# INLINEABLE mkTimeDepositValidator #-}
mkTimeDepositValidator :: TimeDepositParams -> TimeDepositDatum -> TimeDepositRedeemer -> ScriptContext -> Bool
mkTimeDepositValidator params dat red ctx =
  case red of
    -- User can redeem only after the deadline has passed.
    Redeem -> validClaim info False dat (ddBeneficiary dat) (ddDeadline dat)
    -- Collector can collect donations (with no TimeDepositDatum) anytime or
    -- after the collection time with a TimeDepositDatum (unlcaimed time-lock deposits).
    Collect -> validClaim info True dat (dpCollector params) (dpCollectionTime params)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

{-# INLINEABLE validClaim #-}
validClaim :: TxInfo -> Bool -> TimeDepositDatum -> PubKeyHash -> POSIXTime -> Bool
validClaim info iscoll dat pkh pt = do
  case dat of
    TimeDepositDatum {} -> traceIfFalse "time not reached" timeReached
    UnitDatum {} -> iscoll -- Any other datum is handled as donation, meaning only the collector can claim it.
  && traceIfFalse "signature is missing" signedByValidKey
  where
    signedByValidKey :: Bool
    signedByValidKey = txSignedBy info pkh

    timeReached :: Bool
    timeReached = contains (from pt) $ txInfoValidRange info

------------------
{-# INLINEABLE mkWrappedTimeDepositValidator #-}
mkWrappedTimeDepositValidator :: TimeDepositParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedTimeDepositValidator = wrapValidator . mkTimeDepositValidator

validator :: TimeDepositParams -> Validator
validator params =
  mkValidatorScript $
    $$(PlutusTx.compile [||mkWrappedTimeDepositValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE mkWrappedValidatorLucid #-}
--                         Coll PKH       Coll time      datum          redeemer       context
mkWrappedValidatorLucid :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidatorLucid pkh ct = wrapValidator $ mkTimeDepositValidator tdp
  where
    tdp =
      TimeDepositParams
        { dpCollector = PubKeyHash $ unsafeFromBuiltinData pkh, --
          dpCollectionTime = POSIXTime $ unsafeFromBuiltinData ct -- The time the collector can collect donations.
        }

--                             Coll PKH       Coll time      datum          redeemer       context
validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(compile [||mkWrappedValidatorLucid||])

---------------------------------------------------------------------------------------------------
------------------------------------- SAVE VALIDATOR -------------------------------------------

-- saveTimeDepositCode :: IO ()
-- saveTimeDepositCode = writeValidatorToFile "contracts/03-time-deposit.plutus" validatorCode

saveTimeDepositScript :: PubKeyHash -> POSIXTime -> IO ()
saveTimeDepositScript pkh ct = do
  let
  writeValidatorToFile fp $ validator op
  where
    op =
      TimeDepositParams
        { dpCollector = pkh,
          dpCollectionTime = ct
        }
    fp = printf "contracts/03-time-deposit-%s-%s.plutus" (take 8 (show pkh)) -- (show pkh) $ show (getPOSIXTime ct)

---------------------------------------------------------------------------------------------------
---------------------------- HELPER FUNCTIONS FOR BOOTSTRAPPING -----------------------------------

-- Get to script address the time lock deposit sent to.
timeDepositAddressBech32 :: Network -> TimeDepositParams -> String
timeDepositAddressBech32 network tdp = validatorAddressBech32 network (validator tdp)

-- Generate a datum to lock.
printTimeDepositDatumJSON :: PubKeyHash -> String -> IO ()
printTimeDepositDatumJSON pkh time =
  printDataToJSON $
    TimeDepositDatum
      { ddBeneficiary = pkh,
        ddDeadline = fromJust $ posixTimeFromIso8601 time
      }

writeTimeDepositDatumJson :: String -> String -> IO ()
writeTimeDepositDatumJson pkh time = do
  let
  writeDataToFile fp datum
  where
    datum =
      TimeDepositDatum
        { ddBeneficiary = PubKeyHash $ BuiltinByteString $ BS8.pack pkh,
          ddDeadline = fromJust $ posixTimeFromIso8601 time
        }
    fp = printf "contracts/03-time-deposit-%s-datum.json" (take 8 (show pkh))

-- Write unit to construct outputs
writeUnit :: IO ()
writeUnit = writeDataToFile fp ()
  where
    fp = printf "contracts/03-time-deposit-unit.json"

{-
> import qualified Data.ByteString.Char8 as BS8 (pack)
> import Data.Maybe (fromJust)
> import Plutus.V2.Ledger.Api
> import PlutusTX.Builtins.Internal
> import PlutusTx.Builtins.Internal
> TimeDepositDatum (PubKeyHash "alma") (fromJust $ posixTimeFromIso8601 "2022-09-09T08:06:21.630747Z") (BuiltinByteString $ BS8.pack "adatag")
TimeDepositDatum {ddBeneficiary = 616c6d61, ddDeadline = POSIXTime {getPOSIXTime = 1662710781631}, ddAdatag = "adatag"}
-}

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