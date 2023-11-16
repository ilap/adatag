{-# LANGUAGE DataKinds #-}
-- editorconfig-checker-disable-file
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
-- Prevent unboxing, which the plugin can't deal with
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Utilities.Utils where

import Plutus.V1.Ledger.Value -- (CurrencySymbol (CurrencySymbol), unCurrencySymbol,AssetClass (AssetClass), TokenName (TokenName, unTokenName), Value, adaSymbol, flattenValue, singleton, toString, unAssetClass)
import Plutus.V2.Ledger.Api -- ( ValidatorHash (ValidatorHash), ScriptContext (scriptContextTxInfo), TxInfo (txInfoMint), to)

-- (map, mapMaybe, take, takeByteString, traceError)
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins
import PlutusTx.Prelude
import qualified Data.ByteString.Char8 as BS8
import qualified Data.String as Haskell
import Utilities.Conversions
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))

hexToBS :: Haskell.String -> BuiltinByteString
hexToBS s = BuiltinByteString $ bytesFromHex (BS8.pack s)

------- Value related functions
{-# INLINEABLE getTokenNamesOfSymbol #-}
getTokenNamesOfSymbol :: Value -> CurrencySymbol -> [TokenName]
getTokenNamesOfSymbol (Value mp) cur =
  maybe [] Map.keys (Map.lookup cur mp)

-- case Map.lookup cur mp of
-- -> Nothing -> []
-- Just i  -> Map.keys i

{-# INLINEABLE getTokenNameOfSymbol #-}
getTokenNameOfSymbol :: Value -> CurrencySymbol ->  TokenName
getTokenNameOfSymbol (Value mp) cur =
  case Map.lookup cur mp of
    _ -> traceError "expected valid currency symbole"
    Just i -> case Map.keys i of
      [tn] -> tn
      _ -> traceError "expected only one token name"

-- https://github.com/input-output-hk/plutus/blob/c5c1c39cf712fc3cd758a078467277bb2785cdf5/plutus-tx/src/PlutusTx/AssocMap.hs#L265
{-# INLINEABLE getOnlyTokenBySymbol #-}
getOnlyTokenBySymbol :: Value -> CurrencySymbol -> TokenName
getOnlyTokenBySymbol (Value v) s =
  case Map.lookup s v of
    Nothing -> traceError "expected currency in value"
    Just i -> case Map.keys i of
      [tn] -> tn
      _ -> traceError "expected only one token name"

-- https://github.com/input-output-hk/plutus/blob/c5c1c39cf712fc3cd758a078467277bb2785cdf5/plutus-ledger-api/src/PlutusLedgerApi/V1/Value.hs#L256
{-# INLINEABLE hasSymbol #-}
hasSymbol :: Value -> CurrencySymbol -> Bool
hasSymbol (Value mp) cur = case Map.lookup cur mp of
  Nothing -> False
  Just _ -> True

symbolToValidatorHash :: CurrencySymbol -> ValidatorHash
symbolToValidatorHash symbol = ValidatorHash $ unCurrencySymbol symbol

{-
  It validate the username which can have only lower case letters, digits, hyphen
  and underscore (these are all ASCII characters i.e. only 7-bits, < 128).

  Therefore no any UTF-8 conversation is necessery as ASCII characters
  represented in it as they are.

  A username is valid only when:
  1. it's not empty string
  2. max 16 letters long
  3. it's first is any of the lowercase letters
  4. last chars cannot be "-", "_"
  5. only contains "a".."z", "0".."9", "_", "-" letters.
-}
{-# INLINEABLE isValidUsername #-}
isValidUsername :: BuiltinByteString -> Bool
isValidUsername bs =
  let n = lengthOfByteString bs
   in n > 0 && n < 16 --  must have valid length
--        && isLowerCase (indexByteString bs 0) -- 1st char must be letter
--        && isLowerCaseOrDigit (indexByteString bs (n - 1)) -- Last char must be letters or digits
--        && hasOnlyAllowedChars bs -- otherwise only allowed chars.

{-# INLINEABLE isLowerCase #-}
isLowerCase :: Integer -> Bool
isLowerCase ch = (ch >= 97) && (ch <= 122)

{-# INLINEABLE isDigit #-}
isDigit :: Integer -> Bool
isDigit digit = (digit >= 48) && (digit <= 57)

{-# INLINEABLE isLowerCaseOrDigit #-}
isLowerCaseOrDigit :: Integer -> Bool
isLowerCaseOrDigit ch = isLowerCase ch || isDigit ch

{-# INLINEABLE isHyphen #-}
isHyphen :: Integer -> Bool
isHyphen char = char == 45

{-# INLINEABLE isUnderscore #-}
isUnderscore :: Integer -> Bool
isUnderscore char = char == 95

{-# INLINEABLE isValidChar #-}
isValidChar :: Integer -> Bool
isValidChar ch =
  isLowerCase ch
    || isDigit ch
    || isHyphen ch
    || isUnderscore ch

{-# INLINEABLE hasOnlyAllowedChars #-}
hasOnlyAllowedChars :: BuiltinByteString -> Bool
hasOnlyAllowedChars bs =
  let n = lengthOfByteString bs
   in all (isValidChar . indexByteString bs) [0 .. n -1]

--  case lengthOfByteString bs of
--    0 -> True
--    n -> all (isValidChar . indexByteString bs) [0 .. n -1]
