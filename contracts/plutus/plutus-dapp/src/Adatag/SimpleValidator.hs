{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Adatag.SimpleValidator where


import Utilities.Serialise
import Prelude (IO)
import qualified IntegriTree.Proofs as LT
import qualified Prelude as Haskell
import IntegriTree.Val
import IntegriTree.Proofs
import PlutusTx.Builtins
import PlutusTx
import PlutusLedgerApi.V2
import PlutusTx.Bool
import Utilities.PlutusTx (wrapValidator)
import IntegriTree
import qualified PlutusTx.Prelude as PlutusTx
---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / StateHolder ----------------------------------------

data SimpleRedeemer = SimpleRedeemer
  { mrAdatag :: BuiltinByteString
  , mrTreeSize :: Integer
  , mrOldState :: Hash
  , mrNewState :: Hash
  , mrUpdateVal :: Val
  , mrAppendVal :: Val
  , mrProof :: ProofTree
  }
  deriving (Haskell.Show)

PlutusTx.unstableMakeIsData ''SimpleRedeemer

{-# INLINABLE mkTypedSimpleValidator #-}
mkTypedSimpleValidator :: BuiltinData -> SimpleRedeemer -> ScriptContext -> Bool
mkTypedSimpleValidator _ red _ = 
    let state = mrOldState red
        state' = mrNewState red
        ts = mrTreeSize red
    in LT.checkAppend (mrAdatag red) ts state state' (mrUpdateVal red) (mrAppendVal red) (mrProof red)


{-# INLINABLE mkUntypedSimpleValidator1 #-}
mkUntypedSimpleValidator1 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntypedSimpleValidator1 = wrapValidator mkTypedSimpleValidator

{-# INLINEABLE mkUntypedSimpleValidator #-}
mkUntypedSimpleValidator ::  BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntypedSimpleValidator datum redeemer ctx =
    PlutusTx.check (mkTypedSimpleValidator
        (PlutusTx.unsafeFromBuiltinData datum)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData ctx))

simpleValidator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
simpleValidator =
  $$(PlutusTx.compile [||mkUntypedSimpleValidator||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------


saveScript :: IO ()
saveScript = writeCodeToFile "contracts/07_simple_validator.plutus" simpleValidator