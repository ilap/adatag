{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Prelude (($))
import Prelude qualified as Haskell
import PlutusTx.Integer
import Control.Monad (replicateM)
import LabeledTree.Val (hashVal)

import Data.Functor.Identity (Identity)
import Cardano.Ledger.BaseTypes qualified as Alonzo
import Cardano.Ledger.Coin
import Cardano.Ledger.Crypto (StandardCrypto)

import Plutus.Model.Mock.MockConfig
import Plutus.Model.Mock
-- import Cardano.Api (AlonzoEra)
import Plutus.Model.Mock.ProtocolParameters

import System.IO.Unsafe (unsafePerformIO)
-- import PlutusTx.Prelude
import Cardano.Ledger.Alonzo.PParams qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Alonzo 
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams qualified as Babbage
import qualified Cardano.Ledger.Core as C
import Debug.Trace
-- import Data.Coerce (coerce)

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Testing labeled tree logic"
      [good "labeled tree" testProof]
  where
    bad msg = good msg Haskell.. mustFail
    good = testNoErrorsTrace (adaValue 10_000_000_000) $ defaultMockConfig newBabbageParams

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------
pl = ProofLeaf emptyHash

initialTree :: Hash -> ProofTree
-- initialTree v = ProofNode v (ProofLeaf emptyHash) (ProofLeaf emptyHash)
initialTree v = ProofNode ( hashVal Haskell.$ Val 2 "ib15" "ib16") pl (ProofNode ( hashVal Haskell.$ Val 2 "ib14" "ib15") (ProofNode ( hashVal Haskell.$ Val 5 "ib13" "ib14") pl (ProofNode ( hashVal Haskell.$ Val 8 "ib12" "ib13") (ProofNode ( hashVal Haskell.$ Val 17 "ib11" "ib12") pl (ProofNode ( hashVal Haskell.$ Val 32 "ib10" "ib11") (ProofNode ( hashVal Haskell.$ Val 65 "ib9" "ib10") pl (ProofNode ( hashVal Haskell.$ Val 128 "ib8" "ib9") (ProofNode ( hashVal Haskell.$ Val 257 "ib7" "ib8") pl (ProofNode ( hashVal Haskell.$ Val 512 "ib6" "ib7") (ProofNode ( hashVal Haskell.$ Val 1025 "ib5" "ib6") pl (ProofNode ( hashVal Haskell.$ Val 2048 "ib4" "ib5") (ProofNode ( hashVal Haskell.$ Val 4097 "ib3" "ib4") pl (ProofNode ( hashVal Haskell.$ Val 8192 "ib2" "ib3") (ProofNode ( hashVal Haskell.$ Val 16385 "ib1" "ib2") pl (ProofNode ( hashVal Haskell.$ Val 32768 "ib0" "ib1") (ProofNode v pl pl) pl)) pl)) pl)) pl)) pl)) pl)) pl)) pl)

addedTree :: Hash -> Hash -> ProofTree
-- addedTree v1 v2 = ProofNode v1
--              (ProofNode v2 (ProofLeaf emptyHash) (ProofLeaf emptyHash))
--              (ProofLeaf emptyHash)
addedTree v1 v2 = ProofNode ( hashVal Haskell.$ Val 2 "ib15" "ib16") pl (ProofNode ( hashVal Haskell.$ Val 2 "ib14" "ib15") (ProofNode ( hashVal Haskell.$ Val 5 "ib13" "ib14") pl (ProofNode ( hashVal Haskell.$ Val 8 "ib12" "ib13") (ProofNode ( hashVal Haskell.$ Val 17 "ib11" "ib12") pl (ProofNode ( hashVal Haskell.$ Val 32 "ib10" "ib11") (ProofNode ( hashVal Haskell.$ Val 65 "ib9" "ib10") pl (ProofNode ( hashVal Haskell.$ Val 128 "ib8" "ib9") (ProofNode ( hashVal Haskell.$ Val 257 "ib7" "ib8") pl (ProofNode ( hashVal Haskell.$ Val 512 "ib6" "ib7") (ProofNode ( hashVal Haskell.$ Val 1_025 "ib5" "ib6") pl (ProofNode ( hashVal Haskell.$ Val 2_048 "ib4" "ib5") (ProofNode ( hashVal Haskell.$ Val 4_097 "ib3" "ib4") pl (ProofNode ( hashVal Haskell.$ Val 8_192 "ib2" "ib3") (ProofNode ( hashVal Haskell.$ Val 16_385 "ib1" "ib2") pl (ProofNode ( hashVal Haskell.$ Val 32_768 "ib0" "ib1") (ProofNode v1 (ProofNode v2 pl pl) pl) pl)) pl)) pl)) pl)) pl)) pl)) pl)) pl)

n = 15 :: Integer

tsize = 2 Haskell.^ (n Haskell.+ 1)

tsize' = 2 Haskell.* tsize

cnftToken = "i"

adatag = "ilapilapilapila"

initVal = Val tsize "h" "j"

updateVal = Val tsize "h" adatag

appendVal = Val tsize "h" adatag

leafVal = Val tsize' adatag "j"

treeState = rootHash' $ initialTree $ hashVal initVal

treeState' = rootHash' $ addedTree (hashVal updateVal) (hashVal leafVal)

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
      red = createSimpleRedeemer adatag tsize' treeState treeState' initVal initVal $ initialTree $ hashVal initVal

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

{-# NOINLINE newCfg #-}
newCfg :: PParams
newCfg = unsafePerformIO $ readBabbageParams "./babbage-params.json"


-- BabbageParams (C.PParams (BabbageEra StandardCrypto))
-- customBabbageParams :: ( C.PParams (BabbageEra StandardCrypto) -> C.PParams (BabbageEra StandardCrypto) ) -> PParams
-- customBabbageParams f = BabbageParams (f defaultBabbageParams')
{-
myCustomBabbageParams :: C.PParams (BabbageEra StandardCrypto) -> C.PParams (BabbageEra StandardCrypto)
myCustomBabbageParams params = params
      { 
        Babbage.bppNOpt = 600

      }

newParams :: PParams
newParams = customBabbageParams myCustomBabbageParams
-}

modifyBabbageParams :: PParams -> PParams
modifyBabbageParams (BabbageParams bpp) =
  BabbageParams $ case bpp of
    C.PParams ppBabbage ->
      C.PParams $ ppBabbage
        { Babbage.bppMaxTxExUnits = Alonzo.OrdExUnits $ Alonzo.ExUnits 1400000000 1000000000000
        , Babbage.bppMaxBlockExUnits = Alonzo.OrdExUnits $ Alonzo.ExUnits 6200000000 2000000000000
        }
    _ -> bpp
modifyBabbageParams params = params


-- Usage
newBabbageParams :: PParams
newBabbageParams = modifyBabbageParams defaultBabbageParams