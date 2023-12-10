{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use underscore" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Adatag.SimpleValidator
import LabeledTree (Hash, ProofTree (..), Val (..), emptyHash, rootHash')
import Plutus.Model
import Plutus.Model.V2 (mkTypedValidator)
import PlutusLedgerApi.V2
import System.IO
import Test.Tasty (defaultMain, testGroup)

import Prelude (Bool (..), Show (show), ($))
import Prelude qualified as Haskell
import PlutusTx.Integer
import Control.Monad (replicateM)

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Testing labeled tree logic"
      [good "labeled tree" testProof]
  where
    bad msg = good msg Haskell.. mustFail
    good = testNoErrorsTrace (adaValue 10_000_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------
pl = ProofLeaf emptyHash

initialTree :: Val -> ProofTree
-- initialTree v = ProofNode v (ProofLeaf emptyHash) (ProofLeaf emptyHash)
initialTree v = ProofNode (Val 2 "ib15" "ib16") pl (ProofNode (Val 2 "ib14" "ib15") (ProofNode (Val 5 "ib13" "ib14") pl (ProofNode (Val 8 "ib12" "ib13") (ProofNode (Val 17 "ib11" "ib12") pl (ProofNode (Val 32 "ib10" "ib11") (ProofNode (Val 65 "ib9" "ib10") pl (ProofNode (Val 128 "ib8" "ib9") (ProofNode (Val 257 "ib7" "ib8") pl (ProofNode (Val 512 "ib6" "ib7") (ProofNode (Val 1025 "ib5" "ib6") pl (ProofNode (Val 2048 "ib4" "ib5") (ProofNode (Val 4097 "ib3" "ib4") pl (ProofNode (Val 8192 "ib2" "ib3") (ProofNode (Val 16385 "ib1" "ib2") pl (ProofNode (Val 32768 "ib0" "ib1") (ProofNode v pl pl) pl)) pl)) pl)) pl)) pl)) pl)) pl)) pl)

addedTree :: Val -> Val -> ProofTree
-- addedTree v1 v2 = ProofNode v1
--              (ProofNode v2 (ProofLeaf emptyHash) (ProofLeaf emptyHash))
--              (ProofLeaf emptyHash)
addedTree v1 v2 = ProofNode (Val 2 "ib15" "ib16") pl (ProofNode (Val 2 "ib14" "ib15") (ProofNode (Val 5 "ib13" "ib14") pl (ProofNode (Val 8 "ib12" "ib13") (ProofNode (Val 17 "ib11" "ib12") pl (ProofNode (Val 32 "ib10" "ib11") (ProofNode (Val 65 "ib9" "ib10") pl (ProofNode (Val 128 "ib8" "ib9") (ProofNode (Val 257 "ib7" "ib8") pl (ProofNode (Val 512 "ib6" "ib7") (ProofNode (Val 1_025 "ib5" "ib6") pl (ProofNode (Val 2_048 "ib4" "ib5") (ProofNode (Val 4_097 "ib3" "ib4") pl (ProofNode (Val 8_192 "ib2" "ib3") (ProofNode (Val 16_385 "ib1" "ib2") pl (ProofNode (Val 32_768 "ib0" "ib1") (ProofNode v1 (ProofNode v2 pl pl) pl) pl)) pl)) pl)) pl)) pl)) pl)) pl)) pl)

n = 15 :: Integer

tsize = 2 Haskell.^ (n Haskell.+ 1)

tsize' = 2 Haskell.* tsize

cnftToken = "i"

adatag = "ilapilapilapila"

initVal = Val tsize "h" "j"

updateVal = Val tsize "h" adatag

appendVal = Val tsize "h" adatag

leafVal = Val tsize' adatag "j"

treeState = rootHash' $ initialTree initVal

treeState' = rootHash' $ addedTree updateVal leafVal

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------
-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1_000_000_000)

createSimpleRedeemer :: BuiltinByteString -> Integer -> Hash -> Hash -> Val -> Val -> ProofTree -> SimpleRedeemer
createSimpleRedeemer x ts st st' uv av p =
  SimpleRedeemer
    { mrAdatag = x,
      mrTreeSize = ts,
      mrOldState = st,
      mrNewState = st',
      mrUpdateVal = uv,
      mrAppendVal = av,
      mrProof = p
    }

---------------------------------------------------------------------------------------------------
-------------------------------------- @adatag VALIDATORS -----------------------------------------

type SimpleScript = TypedValidator () SimpleRedeemer

validatorScript :: SimpleScript
validatorScript = mkTypedValidator simpleValidator

---------------------------------------------------------------------------------------------------
------------------------------------------- UNIT TESTS --------------------------------------------

testProof :: Run ()
testProof = do
  users <- setupUsers
  let [u1, u2, u3] = users
      val = adaValue 100

  sp <- spend u1 val
  tx <- signTx u1 $ initTx sp val
  submitTx u1 tx

  utxos <- utxoAt validatorScript
  let [(vestRef, vestOut)] = utxos
      red = createSimpleRedeemer adatag tsize' treeState treeState' initVal initVal $ initialTree initVal

      tx = spendingTx u2  red vestRef val
  submitTx u2 tx


initTx :: UserSpend -> Value -> Tx
initTx usp val =
  Haskell.mconcat
    [ userSpend usp,
      payToScript validatorScript (InlineDatum ()) val
    ]

spendingTx ::  PubKeyHash  -> SimpleRedeemer -> TxOutRef -> Value -> Tx
spendingTx pkh red vestRef vestVal =
  Haskell.mconcat
    [ spendScript validatorScript vestRef red ()
    , payToKey pkh vestVal
    ]
