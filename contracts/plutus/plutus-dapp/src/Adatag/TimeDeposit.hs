{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Adatag.TimeDeposit where

import Data.ByteString.Char8 qualified as BS8
import Data.Maybe (fromJust)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V2 (
  Datum (Datum),
  OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
  POSIXTime (..),
  PubKeyHash (..),
  ScriptContext (scriptContextTxInfo),
  TxInfo (txInfoValidRange),
  from,
 )
import PlutusLedgerApi.V2.Contexts (txSignedBy)
import PlutusTx (
  CompiledCode,
  FromData (fromBuiltinData),
  compile,
  liftCode,
  makeLift,
  unstableMakeIsData,
 )
import PlutusTx qualified
import PlutusTx.Builtins.Internal
import PlutusTx.Prelude (
  Bool (..),
  Maybe (..),
  traceError,
  traceIfFalse,
  ($),
  (&&),
  (.),
 )
import Text.Printf (printf)
import Utilities (
  Network,
  posixTimeFromIso8601,
  printDataToJSON,
  validatorAddressBech32,
  wrapValidator,
  writeCodeToFile,
  writeDataToFile,
 )
import Prelude (IO, Show (show), String)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data TimeDepositDatum
  = TimeDepositDatum
      { ddBeneficiary :: PubKeyHash -- Beneficiary of the locked time deposit.
      , ddDeadline :: POSIXTime -- Deadline to claim deposits. Preferably ~20 days (minting policy's parameter)
      -- from submitting the adatag minting transaction
      -- Note:  The minting policy will validate the deadline at minting time.
      -- This prevents any adversaries generating 26 adatags at the same time but having only one time-lock output in the transaction.
      -- Minting policy handles this logic. It's not required for unlocking.
      -- TODO: ddAdatag :: BuiltinByteString - inf future version we would allow multiple minting in the same tx.
      }
  | -- Note: the unit type () has a different signature than the {} and therefore different PLC data
    -- () = {"constructor": 1, "fields": [{"constructor": 0,"fields": []}]}"
    -- {} = {"constructor": 1, "fields": []}"
    UnitDatum ()
  deriving (Prelude.Show)

unstableMakeIsData ''TimeDepositDatum

data TimeDepositParams = TimeDepositParams
  { dpCollector :: PubKeyHash -- Collecting donations (void datum is used) and unclaimed deposits (after a year or two)
  , dpCollectionTime :: POSIXTime -- The time the collector can collect the unclaimed time-lock deposits. It should be twice as deactivation time. ~1-2yrs
  }
  deriving (Prelude.Show)

PlutusTx.makeLift ''TimeDepositParams

-- Collector can collect any donation (having any non TimeDepositDatum) anytime or
-- after the collection time has passed with an UTxO having a valid TimeDepositDatum datum.
-- The beneficiary always can redeem after the deatline in the datum has passed
data TimeDepositRedeemer = Collect | Redeem

unstableMakeIsData ''TimeDepositRedeemer

-- Only inline datums are allowed.
{-# INLINABLE parseTimeDepositDatum #-}
parseTimeDepositDatum :: OutputDatum -> Maybe TimeDepositDatum
parseTimeDepositDatum o = case o of
  NoOutputDatum -> traceError "Found time deposit output but NoOutputDatum"
  OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d -- Inline datum
  OutputDatumHash _ -> traceError "Found time deposit output but no Inline datum"

-- TimeDeposit validator for locking a certain amount of ADA for some time (20 days) at minting time
-- to prevent for buying a lot of the rare usernames and sell them on the market for high price.
{-# INLINABLE timeDepositTypedValidator #-}
timeDepositTypedValidator :: TimeDepositParams -> TimeDepositDatum -> TimeDepositRedeemer -> ScriptContext -> Bool
timeDepositTypedValidator params dat red ctx =
  case red of
    -- User can redeem only after the deadline has passed.
    -- The false means it's not collection but redeem.
    Redeem -> validClaim info False dat (ddBeneficiary dat) (ddDeadline dat)
    -- Collector can collect donations (with no TimeDepositDatum) anytime or
    -- after the collection time with a TimeDepositDatum (unlcaimed time-lock deposits).
    Collect -> validClaim info True dat (dpCollector params) (dpCollectionTime params)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

{-# INLINABLE validClaim #-}
validClaim :: TxInfo -> Bool -> TimeDepositDatum -> PubKeyHash -> POSIXTime -> Bool
validClaim info iscoll dat pkh pt =
  case dat of
    TimeDepositDatum {} -> traceIfFalse "time not reached" timeReached
    _ -> iscoll -- Any other datum is handled as donation, meaning only the collector can claim it.
    -- It's anti pattern but we allow any wrongly formed datums for collections
    && traceIfFalse "signature is missing" signedByValidKey
  where
    signedByValidKey :: Bool
    signedByValidKey = txSignedBy info pkh

    timeReached :: Bool
    timeReached = contains (from pt) $ txInfoValidRange info

{-# INLINABLE timeDepositUntypedValidator #-}
timeDepositUntypedValidator :: TimeDepositParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
timeDepositUntypedValidator = wrapValidator . timeDepositTypedValidator

timeDepositValidator :: TimeDepositParams -> PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
timeDepositValidator params =
  $$(PlutusTx.compile [||timeDepositUntypedValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params

---------------------------------------------------------------------------------------------------
------------------------------------- SAVE VALIDATOR -------------------------------------------

saveTimeDepositValidator :: PubKeyHash -> POSIXTime -> IO ()
saveTimeDepositValidator pkh ct = do
  let
  writeCodeToFile fp $ timeDepositValidator op
  where
    op =
      TimeDepositParams
        { dpCollector = pkh
        , dpCollectionTime = ct
        }
    fp = printf "contracts/03-time-deposit-%s-%s.plutus" (show pkh) (show (getPOSIXTime ct))

---------------------------------------------------------------------------------------------------
---------------------------- HELPER FUNCTIONS FOR BOOTSTRAPPING -----------------------------------

-- Get to script address the time lock deposit sent to.
timeDepositAddressBech32 :: Network -> TimeDepositParams -> String
timeDepositAddressBech32 network tdp = validatorAddressBech32 network (timeDepositValidator tdp)

-- Generate a datum to lock.
printTimeDepositDatumJSON :: PubKeyHash -> String -> IO ()
printTimeDepositDatumJSON pkh time =
  printDataToJSON
    $ TimeDepositDatum
      { ddBeneficiary = pkh
      , ddDeadline = fromJust $ posixTimeFromIso8601 time
      }

writeTimeDepositDatumJson :: String -> String -> IO ()
writeTimeDepositDatumJson pkh time = do
  let
  writeDataToFile fp datum
  where
    datum =
      TimeDepositDatum
        { ddBeneficiary = PubKeyHash $ BuiltinByteString $ BS8.pack pkh
        , ddDeadline = fromJust $ posixTimeFromIso8601 time
        }
    fp = printf "contracts/03-time-deposit-%s-datum.json" $ show pkh

-- Write unit to construct outputs
writeUnit :: IO ()
writeUnit = writeDataToFile fp ()
  where
    fp = printf "contracts/03-time-deposit-unit.json"

{-
> import qualified Data.ByteString.Char8 as BS8 (pack)
> import Data.Maybe (fromJust)
> import PlutusLedgerApi.V2
> import PlutusTX.Builtins.Internal
> import PlutusTx.Builtins.Internal
> TimeDepositDatum (PubKeyHash "alma") (fromJust $ posixTimeFromIso8601 "2022-09-09T08:06:21.630747Z") (BuiltinByteString $ BS8.pack "adatag")
TimeDepositDatum {ddBeneficiary = 616c6d61, ddDeadline = POSIXTime {getPOSIXTime = 1662710781631}, ddAdatag = "adatag"}

See an exampe below, how to use it
Repl> import PlutusLedgerApi.V2
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
