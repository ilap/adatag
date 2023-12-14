{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BangPatterns #-}

module Adatag.Utils where

import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins
import PlutusTx.Prelude

{-# INLINABLE getTokenNamesOfSymbol #-}
getTokenNamesOfSymbol :: Value -> CurrencySymbol -> [TokenName]
getTokenNamesOfSymbol (Value mp) cur =
  maybe [] Map.keys $ Map.lookup cur mp

-- https://github.com/input-output-hk/plutus/blob/c5c1c39cf712fc3cd758a078467277bb2785cdf5/plutus-tx/src/PlutusTx/AssocMap.hs#L265
{-# INLINABLE getOnlyTokenBySymbol #-}
getOnlyTokenBySymbol :: Value -> CurrencySymbol -> TokenName
getOnlyTokenBySymbol (Value v) s =
  case Map.lookup s v of
    Nothing -> traceError "expected currency in value"
    Just i -> case Map.keys i of
      [tn] -> tn
      _ -> traceError "expected only one token name"

-- https://github.com/input-output-hk/plutus/blob/c5c1c39cf712fc3cd758a078467277bb2785cdf5/plutus-ledger-api/src/PlutusLedgerApi.V1/V1/Value.hs#L256
{-# INLINABLE hasSymbol #-}
hasSymbol :: Value -> CurrencySymbol -> Bool
hasSymbol (Value mp) cur = isJust $ Map.lookup cur mp

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
{-# INLINABLE isValidUsername #-}
isValidUsername :: BuiltinByteString -> Bool
isValidUsername bs =
  let n = lengthOfByteString bs
   in n > 0 && n < 16 --  must have valid length
          && isLowerCase (indexByteString bs 0) -- 1st char must be letter
          && isLowerCaseOrDigit (indexByteString bs (n - 1)) -- Last char must be letters or digits
          && hasOnlyAllowedChars bs -- otherwise only allowed chars.

{-# INLINABLE isLowerCase #-}
isLowerCase :: Integer -> Bool
isLowerCase ch = (ch >= 97) && (ch <= 122)

{-# INLINABLE isDigit #-}
isDigit :: Integer -> Bool
isDigit digit = (digit >= 48) && (digit <= 57)

{-# INLINABLE isLowerCaseOrDigit #-}
isLowerCaseOrDigit :: Integer -> Bool
isLowerCaseOrDigit ch = isLowerCase ch || isDigit ch

{-# INLINABLE isHyphen #-}
isHyphen :: Integer -> Bool
isHyphen char = char == 45

{-# INLINABLE isUnderscore #-}
isUnderscore :: Integer -> Bool
isUnderscore char = char == 95

{-# INLINABLE isValidChar #-}
isValidChar :: Integer -> Bool
isValidChar ch =
  isLowerCase ch
    || isDigit ch
    || isHyphen ch
    || isUnderscore ch

{-# INLINABLE hasOnlyAllowedChars #-}
hasOnlyAllowedChars :: BuiltinByteString -> Bool
hasOnlyAllowedChars bs = go 0
  where
    !n = lengthOfByteString bs

    go !i
      | i >= n = True
      | not (isValidChar (indexByteString bs i)) = False
      | otherwise = go (i + 1)
