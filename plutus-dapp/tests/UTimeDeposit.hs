{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import Contracts.TimeDeposit
import qualified Contracts.TimeDeposit as TD
import Control.Monad (replicateM)
import Plutus.Model
import Plutus.V2.Ledger.Api
  ( POSIXTime (POSIXTime, getPOSIXTime),
    TxOut (txOutValue),
    TxOutRef,
    Value (..), PubKeyHash,
  )
import PlutusTx.Prelude (AdditiveSemigroup ((+)), Bool, Integer, ($), (.), (<>))
import System.IO
import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude (Bool (..), mconcat, show)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

type TimeDepositScript = TypedValidator TD.TimeDepositDatum TD.TimeDepositRedeemer

params :: PubKeyHash -> POSIXTime -> TimeDepositParams
params pkh time = do
  TimeDepositParams
    { dpCollector = pkh,
      dpCollectionTime = time
    }

script :: TimeDepositParams -> TimeDepositScript
script tp = TypedValidator $ toV2 $ TD.validator tp

setupThreeUsers :: Run [PubKeyHash]
setupThreeUsers = replicateM 3 $ newUser $ ada (Lovelace 1_000)

main :: IO ()
main = do
  defaultMain $ do
    testGroup
      "Time lock deposit tests"
      [ testGroup
          "All times are in POSIXTime (Not slots)"
          [ timeDeposit defaultBabbage
          ]
      ]

timeDeposit :: MockConfig -> TestTree
timeDeposit cfg = do
  testGroup
    "Testing timedeposit"
    [ testGroup
        "Redeems and Collections"
        -- Test cases
        -- Collection donations or unclaimed deposits:
        -- 1. Donations         : UnitDatum or
        -- 2. Unclaimed deposits: valid TimeDepositDatum and collection time reached
        [ good "Donation |  unit Datum |  raeached | collect " $ testCollections True True True, -- unit datum, must pass.
          good "Donation |  unit Datum | !raeached | collect " $ testCollections True False True, -- unit datum, must pass.
          good "Donation | valid Datum |  raeached | collect " $ testCollections False True True, -- valid dat, must pass, coll time reached.
          bad "Invalid  | valid Datum | !raeached | collect " $ testCollections False False True, -- valid dat, must fail, coll time not reached.
          bad "Invalid  |  unit Datum | raeached  |  redeem " $ testCollections True True False, -- unit datum, must fail.
          bad "Invalid  |  unit Datum | !raeached |  redeem " $ testCollections True False False, -- unit datum, must fail.
          good "Claim    | valid Datum | raeached  |  redeem " $ testCollections False True False, -- valid datum, must pass, deadline reached.
          bad "Invalid  | valid Datum | !raeached |  redeem " $ testCollections False False False -- valid datum, must fail, deadline not reached.
        ],
      bad "None signing" testNoSigning
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors{-Trace-} (adaValue 10_000_000) cfg

--
testCollections :: Bool -> Bool -> Bool -> Run ()
testCollections unit reached collect = do
  users <- setupThreeUsers
  c <- currentTime
  let [u1, _, c1] = users
      dlDays = 20
      ctDays = 183
      -- Curr time range is 5000, 7000, booth inclusive
      -- Rule: the deadline/collection time must be lower or equal with lower bound of curr time range
      dl = getPOSIXTime $ days dlDays -- 20 days
      ct = getPOSIXTime $ days ctDays -- hals years
      deadline = POSIXTime (getPOSIXTime c + dl)
      colltime = POSIXTime (getPOSIXTime c + ct)

      o = if reached then 1 else -1
      day = if collect then ctDays else dlDays
      waitFor = day + o

      dat = if unit then TD.UnitDatum () else vtd
        where
          vtd =
            TimeDepositDatum
              { ddBeneficiary = u1,
                ddDeadline = deadline
              }
      collector = if collect then c1 else u1
      red = if collect then TD.Collect else TD.Redeem
  logInfo $ "Deadline: " <> show deadline <> "\nCollection Time: " <> show colltime
  testTimeDepositColletion collector collector c1 dat red colltime waitFor

testNoSigning :: Run ()
testNoSigning = do
  users <- setupThreeUsers
  let [u1, u2, c1] = users
  testTimeDepositColletion u1 u2 c1 (UnitDatum ()) TD.Collect (POSIXTime 0) 20

testTimeDepositColletion :: PubKeyHash -> PubKeyHash -> PubKeyHash -> TD.TimeDepositDatum -> TD.TimeDepositRedeemer -> POSIXTime -> Integer -> Run ()
testTimeDepositColletion sender receiver collector dat red colltime waitDays = do
  let val = adaValue 100
  let s = script $ params collector colltime

  checkBalance (gives sender val s) $ do
    sp <- spend sender val
    let ltx = lockingTx1 s dat sp val
    submitTx sender ltx
    logInfo $ "Locking Tx: " <> show ltx

  wait $ days waitDays

  utxos <- utxoAt s
  let [(vestRef, vestOut)] = utxos
  checkBalance (gives s (txOutValue vestOut) receiver) $ do
    -- It sets the valid range relative to current time, [curr-x ... curr ... curr++].
    range <- currentTimeInterval (POSIXTime (-999)) (POSIXTime 1_000)
    logInfo $ "Range: " <> show range
    let ctx = claimingTx1 s receiver dat red vestRef (txOutValue vestOut)
    tx <- validateIn range ctx
    submitTx receiver tx

lockingTx1 :: TimeDepositScript -> TD.TimeDepositDatum -> UserSpend -> Value -> Tx
lockingTx1 scr dat usp val =
  mconcat
    [ userSpend usp,
      payToScript scr (InlineDatum dat) val
    ]

claimingTx1 :: TimeDepositScript -> PubKeyHash -> TD.TimeDepositDatum -> TD.TimeDepositRedeemer -> TxOutRef -> Value -> Tx
claimingTx1 scr pkh dat red vestRef vestVal =
  mconcat
    [ spendScript scr vestRef red dat,
      payToKey pkh vestVal
    ]
