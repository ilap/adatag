{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Contracts.ControlNFTMinting where

import qualified Data.ByteString.Char8 as BS8 (pack, unpack)
import Plutus.V1.Ledger.Value (flattenValue)
import Plutus.V2.Ledger.Api (BuiltinData, CurrencySymbol, MintingPolicy, ScriptContext (scriptContextTxInfo), TokenName (unTokenName), TxId (TxId, getTxId), TxInInfo (txInInfoOutRef), TxInfo (txInfoInputs, txInfoMint), TxOutRef (TxOutRef, txOutRefId, txOutRefIdx), mkMintingPolicyScript)
import qualified PlutusTx
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import PlutusTx.Prelude (Bool, Eq ((==)), all, any, elem, head, traceIfFalse, ($), (&&))
import Text.Printf (printf)
import Utilities (bytesFromHex, bytesToHex, currencySymbol, wrapPolicy, writeCodeToFile, writePolicyToFile)
import Prelude (Bool (False), IO, Integer, Show (show), String)

-- One-shot NFT minting policy for generating 26 control NFTs for @adatag namely "a" to "z"
{-# INLINEABLE mkNFTPolicy #-}
mkNFTPolicy :: TxOutRef -> [TokenName] -> () -> ScriptContext -> Bool
mkNFTPolicy oref tnList () ctx =
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

{-# INLINEABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy tid ix tn' = wrapPolicy $ mkNFTPolicy oref tn
  where
    oref :: TxOutRef
    oref =
      TxOutRef
        (TxId $ PlutusTx.unsafeFromBuiltinData tid)
        (PlutusTx.unsafeFromBuiltinData ix)

    tn :: [TokenName]
    tn = PlutusTx.unsafeFromBuiltinData tn'

nftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [||mkWrappedNFTPolicy||])

nftPolicy :: TxOutRef -> [TokenName] -> MintingPolicy
nftPolicy oref tns =
  mkMintingPolicyScript $
    nftCode
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId oref)
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx oref)
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData tns)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveNFTCode :: IO ()
saveNFTCode = writeCodeToFile "assets/02-control-nft-minting.plutus" nftCode

saveNFTPolicy :: TxOutRef -> [TokenName] -> IO ()
saveNFTPolicy oref tn =
  writePolicyToFile
    ( printf
        "assets/02-control-nft-minting-%s#%d-%s.plutus"
        (show $ txOutRefId oref)
        (txOutRefIdx oref)
        tn'
    )
    $ nftPolicy oref tn
  where
    tn' :: String
    tn' = case unTokenName $ head tn of
      (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs

letters :: [TokenName]
letters = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

-- It does not require handling utf-8, as these are ASCII letters.
toBS :: String -> BuiltinByteString
toBS s = BuiltinByteString $ bytesFromHex (BS8.pack s)

mockOref :: String -> Integer -> TxOutRef
mockOref tx = TxOutRef (TxId $ toBS tx)

-- Generate the currency symbol based on the parameters (UTxO ref, and the letters)
nftCurrencySymbol :: TxOutRef -> CurrencySymbol
nftCurrencySymbol oref = currencySymbol $ nftPolicy oref letters