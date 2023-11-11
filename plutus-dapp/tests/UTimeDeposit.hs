{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import qualified Contracts.TimeDeposit as TD
import Control.Monad (replicateM)
import Plutus.Model
import Plutus.Model.Fork.Ledger.Slot (Slot)
import Plutus.V2.Ledger.Api
  ( POSIXTime,
    PubKeyHash,
    TxOut (txOutValue),
    TxOutRef,
    Value (..),
  )
import PlutusTx.Prelude (($), (.), (<>))
import System.IO
import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude (mconcat, show)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

type TimeDepositScript = TypedValidator TD.TimeDepositParams TD.TimeDepositDatum 

script :: TimeDepositScript
script = TypedValidator $ toV2 TD.validator

setupTwoUsers :: Run [PubKeyHash]
setupTwoUsers = replicateM 2 $ newUser $ ada (Lovelace 1_000)

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
        "Beneficiary signing"
        {--
        -- contains [deadline... infinite] [5000 ... 5999]   ----           deadline cMinT cMaxT slot
        -- The used plutus-simple-model's `scSlotZeroTime` is 4999, meaning Slot0=4999, Slot1=5999, and so on.
        -- 6 tests should be suitable:[bef,bef] [before, on deadline], [bef, after deadline], [on deadline, after],[after,after],[on,on]
        -- The interval always must be (current - some time) and (current + some time), otherwise it will fail in Ledger.
        -- E.g.: currslot in posix time 4999, tx intervall 5000<->5001 or even 4997<->4998
        --}
        [ bad "1. bad  -> (bef, bef); CurrSlot: 4999; Deadline: 5000; TxValidRange (4000, 4999)" $ testBeneficiary 5_000 (-999) 0 0,
          bad "2. bad  -> (bef,  on); CurrSlot: 4999; Deadline: 5000; TxValidRange (4000, 5000)" $ testBeneficiary 5_000 (-999) 1 0,
          bad "3. bad  -> (bef, aft); CurrSlot: 4999; Deadline: 5000; TxValidRange (4500, 5500)" $ testBeneficiary 5_000 (-499) 501 0,
          good "4. good -> ( on,  on); CurrSlot: 5999; Deadline: 5000; TxValidRange (5000, 5000)" $ testBeneficiary 5_000 (-999) (-999) 1,
          good "5. good -> ( on, aft); CurrSlot: 5999; Deadline: 5000; TxValidRange (5000, 6000)" $ testBeneficiary 5_000 (-999) 1 1,
          good "6. good -> (aft, aft); CurrSlot: 5999; Deadline: 5000; TxValidRange (5500, 6500)" $ testBeneficiary 5_000 (-499) 501 1
        ],
      bad "None signing" $ testNoSigning 5_000 0 0 0
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

testBeneficiary :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()
testBeneficiary d curMinT curMaxT wSlot = do
  users <- setupTwoUsers
  let [u1, u2] = users
      adatag = "alma"
      dat = TD.TimeDepositDatum u1 d adatag
  testTimeDeposit u1 u2 dat curMinT curMaxT wSlot

testNoSigning :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()
testNoSigning d curMinT curMaxT wSlot = do
  users <- setupTwoUsers
  let [u1, u2] = users
      adatag = "alma"
      dat = TD.TimeDepositDatum u1 d adatag
  testTimeDeposit u2 u2 dat curMinT curMaxT wSlot

testTimeDeposit :: PubKeyHash -> PubKeyHash -> TD.TimeDepositDatum -> POSIXTime -> POSIXTime -> Slot -> Run ()
testTimeDeposit sigUser receiver dat curMinT curMaxT wSlot = do
  let val = adaValue 100
  checkBalance (gives sigUser val script) $ do
    sp <- spend sigUser val
    submitTx sigUser $ lockingTx1 dat sp val

  waitNSlots wSlot

  utxos <- utxoAt script
  let [(vestRef, vestOut)] = utxos
  checkBalance (gives script (txOutValue vestOut) receiver) $ do
    range <- currentTimeInterval curMinT curMaxT
    tx <- validateIn range $ claimingTx1 receiver dat vestRef (txOutValue vestOut)
    -- XXX: only for debugging slots, as the slotcfg could be changed in the future
    -- logError $ show range <> "\n" <> show (TD.ddDeadline dat)
    submitTx sigUser tx

lockingTx1 :: TD.TimeDepositDatum -> UserSpend -> Value -> Tx
lockingTx1 dat usp val =
  mconcat
    [ userSpend usp,
      payToScript script (HashDatum dat) val
    ]

claimingTx1 :: PubKeyHash -> TD.TimeDepositDatum -> TxOutRef -> Value -> Tx
claimingTx1 pkh dat vestRef vestVal =
  mconcat
    [ spendScript script vestRef () dat,
      payToKey pkh vestVal
    ]
