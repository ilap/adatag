{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Main where

import Contracts.AdatagMinting
import Contracts.AlwaysFail
import Contracts.ControlNFTMinting
import Contracts.TimeDeposit
import Contracts.Validator
import Control.Monad (Monad (return), replicateM, unless, void)
import Plutus.Model
import Plutus.V1.Ledger.Value (CurrencySymbol)
import Plutus.V2.Ledger.Api (POSIXTime, PubKeyHash, TokenName, TxOut (txOutValue), TxOutRef, ValidatorHash, Value (..), singleton)
import PlutusTx.Prelude (Bool, Eq ((==)), Integer, Semigroup ((<>)), foldMap, ($), (.))
import System.IO
import Test.Tasty (defaultMain, testGroup)
import Prelude (Bool (..), mconcat)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Testing @adatag's NFT minting (business) logic"
      [ testGroup
          "Deploying tests"
          [good "Test @adatag deploy" $ testBootstrapping True]
      ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors {-Trace-} (adaValue 10_000_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 4 $ newUser $ ada (Lovelace 1_000_000_000)

---------------------------------------------------------------------------------------------------
-------------------------------------- @adatag VALIDATORS -----------------------------------------
-- Always Fail Validator
type AlwaysFailScript = TypedValidator () () -- No datum/redeemer

afvScript :: AlwaysFailScript
afvScript = TypedValidator $ toV2 alwaysFailValidator

-- Controlo NFT

cnftScript :: TxOutRef -> TypedPolicy ()
cnftScript ref = TypedPolicy . toV2 $ cnftPolicy ref letters

-- State Holder Validator
type ValidatorScript = TypedValidator ValidatorDatum () -- No redeemer

validatorScript :: ControlNFT -> ValidatorScript
validatorScript cnft = TypedValidator $ toV2 $ stateHolderValidator cnft

-- Time Deposit Validator

type TimeDepositScript = TypedValidator TimeDepositDatum TimeDepositRedeemer

timeDepositParams :: PubKeyHash -> POSIXTime -> TimeDepositParams
timeDepositParams pkh time = do
  TimeDepositParams
    { dpCollector = pkh,
      dpCollectionTime = time
    }

script :: TimeDepositParams -> TimeDepositScript
script tp = TypedValidator $ toV2 $ timeDepositValidator tp

-- Adatag Minting Policy

type MintingScript = TypedPolicy MintRedeemer

adatagPolicyParams :: CurrencySymbol -> ValidatorHash -> ValidatorHash -> POSIXTime -> Integer -> Integer -> MintParams
adatagPolicyParams cnft shv tdv exp lp md =
  MintParams
    { mpControlNFT = cnft,
      mpValidator = shv,
      mpTimeDepositValidator = tdv,
      mpLockExpiry = exp,
      mpUserLockingPeriod = lp,
      mpMaxDeposit = md
    }

mintingScript :: MintParams -> TypedPolicy ()
mintingScript mp = TypedPolicy . toV2 $ adatagPolicy mp

---------------------------------------------------------------------------------------------------
-------------------------------------- @adatag UNIT TESTS -----------------------------------------

testBootstrapping :: Bool -> Run ()
testBootstrapping b = do
  case b of
    True -> logInfo "True"
    False -> logError "False"