{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Adatag.ControlNFTMinting where

import Data.ByteString.Char8 qualified as BS8 (unpack)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V2
import PlutusTx qualified
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import PlutusTx.Prelude (
  Bool (..),
  Eq ((==)),
  all,
  any,
  elem,
  head,
  traceIfFalse,
  ($),
  (&&),
 )
import PlutusTx.Show qualified as PlutusTx
import Text.Printf (printf)
import Utilities (currencySymbol)
import Utilities.PlutusTx (wrapPolicy)
import Utilities.Serialise (writeCodeToFile)
import Prelude (IO, Show (show), String)

-- One-shot NFT minting policy for generating 26 control NFTs for @adatag namely "a" to "z"
{-# INLINABLE cnftTypedPolicy #-}
cnftTypedPolicy :: TxOutRef -> [TokenName] -> () -> ScriptContext -> Bool
cnftTypedPolicy oref tnList () ctx =
  do
    -- Tracing values inside plutus code
    -- let ownSymbol = ownCurrencySymbol ctx
    -- let c = c2b ownSymbol
    -- traceIfFalse "@@@@@@@@@@@@@@@@@@@ Owncurrency symbol @@@@@@@@@@@@" False
    -- &&
    traceIfFalse "UTxO not consumed" (hasUTxO oref ctx)
    && traceIfFalse "wrong amount minted" (checkMintedAmount tnList ctx)
  where
    -- Check if the UTxO is consumed.
    hasUTxO :: TxOutRef -> ScriptContext -> Bool
    hasUTxO o c = any (\i -> txInInfoOutRef i == o) (txInfoInputs (scriptContextTxInfo c))

    -- Check if the correct amount of NFTs are minted.
    checkMintedAmount :: [TokenName] -> ScriptContext -> Bool
    checkMintedAmount tns c =
      case flattenValue (txInfoMint (scriptContextTxInfo c)) of
        xs -> all (\(_, tn, amt) -> tn `elem` tns && amt == 1) xs

{-# INLINABLE cnftUntypedPolicy #-}
cnftUntypedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
cnftUntypedPolicy tid ix tn = wrapPolicy $ cnftTypedPolicy oref tn'
  where
    oref :: TxOutRef
    oref =
      TxOutRef
        (TxId $ PlutusTx.unsafeFromBuiltinData tid)
        (PlutusTx.unsafeFromBuiltinData ix)

    tn' :: [TokenName]
    tn' = PlutusTx.unsafeFromBuiltinData tn

cnftPolicy :: TxOutRef -> [TokenName] -> PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> ())
cnftPolicy oref tns =
  $$(PlutusTx.compile [||cnftUntypedPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 (PlutusTx.toBuiltinData $ getTxId $ txOutRefId oref)
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 (PlutusTx.toBuiltinData $ txOutRefIdx oref)
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 (PlutusTx.toBuiltinData tns)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Generate the currency symbol based on the parameters (UTxO ref, and the letters)
controlNFTCurrencySymbol :: TxOutRef -> CurrencySymbol
controlNFTCurrencySymbol oref = currencySymbol $ cnftPolicy oref letters

letters :: [TokenName]
letters = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

saveControlNFTMintingPolicy :: TxOutRef -> IO ()
saveControlNFTMintingPolicy oref =
  writeCodeToFile
    ( printf
        "contracts/02-cnft-minting-%s#%d-%s.plutus"
        (show $ txOutRefId oref)
        (txOutRefIdx oref)
        tn'
    )
    $ cnftPolicy oref letters
  where
    tn' :: String
    tn' = case unTokenName $ head letters of
      (BuiltinByteString bs) -> BS8.unpack bs
