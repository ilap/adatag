{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE  InstanceSigs #-}
{-# LANGUAGE  ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}


module Contracts.ControlNFTMinting where

import PlutusCore.Version (plcVersion100)
import qualified Data.ByteString.Char8 as BS8 (unpack)
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V2 (BuiltinData, ScriptContext (scriptContextTxInfo), TokenName (unTokenName), TxId (TxId, getTxId), TxInInfo (txInInfoOutRef), TxInfo (txInfoInputs, txInfoMint), TxOutRef (TxOutRef, txOutRefId, txOutRefIdx), CurrencySymbol)
import qualified PlutusTx
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString), traceAll)
import PlutusTx.Prelude (Bool (True), Eq ((==)), all, any, elem, head, traceIfFalse, ($), (&&), (<>), traceBool, trace, traceIfTrue)
import Text.Printf (printf)
import Utilities ( currencySymbol, policyToScript )
import Utilities.PlutusTx ( wrapPolicy )
import Utilities.Serialise ( writeCodeToFile )
import Prelude (Bool (False), IO, Show (show), String)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import qualified PlutusTx.Show  as PlutusTx
import qualified PlutusTx.Trace as PlutusTx
import qualified PlutusTx.Prelude  as PlutusTx
import PlutusLedgerApi.V2.Contexts
import PlutusLedgerApi.V2
-- import PlutusLedgerApi.V1.Contexts (ownCurrencySymbol)


newtype Cn = Cn BuiltinByteString
PlutusTx.deriveShow ''Cn

-- b2Cn :: BuiltinByteString -> Cn
-- b2Cn a =  Cn a

{-# INLINEABLE c2b #-}
c2b :: CurrencySymbol -> Cn
c2b c = Cn $ unCurrencySymbol c 

-- One-shot NFT minting policy for generating 26 control NFTs for @adatag namely "a" to "z"
{-# INLINEABLE cnftTypedPolicy #-}
cnftTypedPolicy :: TxOutRef -> [TokenName] -> () -> ScriptContext -> Bool
cnftTypedPolicy oref tnList () ctx = do
  -- Tracing values
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
        _ -> False

{-# INLINEABLE cnftUntypedPolicy #-}
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
cnftPolicy oref tns = $$(PlutusTx.compile [||cnftUntypedPolicy||])
     `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 (PlutusTx.toBuiltinData $ getTxId $ txOutRefId oref)
     `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 (PlutusTx.toBuiltinData $ txOutRefIdx oref)
     `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 (PlutusTx.toBuiltinData tns)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Generate the currency symbol based on the parameters (UTxO ref, and the letters)
-- FIXME: It does not work...
controlNFTCurrencySymbol :: TxOutRef -> CurrencySymbol
controlNFTCurrencySymbol oref =  currencySymbol $ cnftPolicy oref letters

letters :: [TokenName]
letters = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

saveControlNFTMintingPolicy :: TxOutRef -> IO ()
saveControlNFTMintingPolicy oref =
  writeCodeToFile
    ( printf
        "contracts/02-control-nft-minting-%s#%d-%s.plutus"
        (show $ txOutRefId oref)
        (txOutRefIdx oref)
        tn'
    )
    $ cnftPolicy oref letters
  where
    tn' :: String
    tn' = case unTokenName $ head letters of -- It gets the first tokenname the "a"
      (BuiltinByteString bs) -> BS8.unpack bs --  $ bytesToHex bs
