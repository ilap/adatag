{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- {-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE BlockArguments        #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Main where

import           Adatag.Utils
import           PlutusLedgerApi.V1.Value (AssetClass (AssetClass),
                                           TokenName (TokenName, unTokenName),
                                           Value, adaSymbol, flattenValue,
                                           singleton, unAssetClass)
import           PlutusLedgerApi.V2       (CurrencySymbol (CurrencySymbol))
import           PlutusTx.Builtins.Class  (stringToBuiltinByteString)
import           PlutusTx.Prelude         (takeByteString, traceError)

-- Define a custom function to retrieve the token name
getTokenName :: CurrencySymbol -> PlutusLedgerApi.V1.Value.Value -> TokenName
getTokenName symbol v = do
  let xs = flattenValue v
  -- let filtered = filter (\(c, _, _) -> c == symbol) xs
  let filtered = filter (\(c, _, _) -> c == symbol) xs
  case filtered of
    [(_, tn, _)] -> tn
    _            -> traceError "expected exactly one token name"

-- Example usage
currencySymbol :: CurrencySymbol
currencySymbol = adaSymbol

assetClass :: AssetClass
assetClass = AssetClass {unAssetClass = (CurrencySymbol "symbol", TokenName "alma")}

stringToNumber :: String -> Int
stringToNumber str =
  let
    maxPrice = 1500 :: Double
    base = 2 :: Double
    priceExp = length str - 1
    number = floor (maxPrice / base ^ priceExp)
  in max number 5


main :: IO ()
main = do
  {-let datum =
        ValidatorDatum
          { vdTreeState = AdatagAdded,
            vdTreeSize = 1,
            vdAdatag = stringToBuiltinByteString "ilap",
            vdTreeProof = stringToBuiltinByteString "1234567890abcdef1234567890abcdef1234567890abcdef"
          }

  putStrLn $ "The tree root hash of the datum is: " ++ show (vdTreeProof datum)
-}
  let st = CurrencySymbol "symbol"
  let v = singleton st (TokenName "tokenname") 1
  let b = stringToBuiltinByteString "t"

  print $ takeByteString 1 (unTokenName (getTokenName st v)) == b

  print $ isValidUsername "0ilap"
  print $ isValidUsername "-ilap"
  print $ isValidUsername "ilap-"
  print $ isValidUsername "ilap1234567890123"
  print $ isValidUsername "_ilap"
  print $ isValidUsername "ilap_"

  print $ isValidUsername "ilap12345678901"
  print $ isValidUsername "ilap1234567890a"
  print $ isValidUsername "i-78901"
  print $ isValidUsername "i-7-__8901"

  print $ stringToNumber "a"
  print $ stringToNumber "ab"
  print $ stringToNumber "abc"
  print $ stringToNumber "abcd"
  print $ stringToNumber "abcde"
  print $ stringToNumber "abcdef"
  print $ stringToNumber "abcdefg"
  print $ stringToNumber "abcdefgh"
  print $ stringToNumber "abcdefghi"
  print $ stringToNumber "abcdefghij"
  print $ stringToNumber "abcdefghijk"
  print $ stringToNumber "abcdefghijkl"



