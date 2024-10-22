{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Main where

import Adatag.ControlNFTMinting qualified as CM
import Control.Monad (Monad (return), replicateM, unless)
import Plutus.Model
import Plutus.Model.Validator.V2 qualified as MV2
import PlutusLedgerApi.V1.Value (CurrencySymbol)
import PlutusLedgerApi.V2 (
  PubKeyHash,
  TokenName,
  TxOut (txOutValue),
  TxOutRef,
  Value (..),
  singleton,
 )
import PlutusTx.Prelude (
  Eq ((==)),
  Semigroup ((<>)),
  foldMap,
  ($),
  (.),
 )
import System.IO
import Test.Tasty (defaultMain, testGroup)
import Prelude (mconcat)

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Testing CNFT minting policy"
      [ good "Must pass - Minting Authorization Tokens works       " testMintControlNFT
      , bad "Must fail - Minting the same NFTs twice fails" testMintControlNFTTwice
      ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors {-Trace-} (adaValue 10_000_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 4 $ newUser $ ada (Lovelace 1_000_000_000)

-- NFT Minting Policy's script
nftScript :: TxOutRef -> [TokenName] -> TypedPolicy ()
nftScript ref tn = MV2.mkTypedPolicy $ CM.cnftPolicy ref tn

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING MINTING AUTH TOKEN -----------------------------------------

mintNFTTx :: TxOutRef -> TxOut -> [TokenName] -> Value -> PubKeyHash -> Tx
mintNFTTx ref out tn val pkh =
  mconcat
    [ mintValue (nftScript ref tn) () val
    , payToKey pkh $ val <> txOutValue out
    , spendPubKey ref
    ]

-- Create a Plutus Value from a list of token names.
valueFromTokenNames :: CurrencySymbol -> [TokenName] -> Value
valueFromTokenNames cs = foldMap (\tn -> singleton cs tn 1)

mintNFT :: PubKeyHash -> Run (CurrencySymbol, Value)
mintNFT u = do
  utxos <- utxoAt u
  let [(ref, out)] = utxos
      currSymbol = scriptCurrencySymbol (nftScript ref CM.letters)
      mintingValue = valueFromTokenNames currSymbol CM.letters
  submitTx u $ mintNFTTx ref out CM.letters mintingValue u
  v1 <- valueAt u
  unless (v1 == adaValue 1_000_000_000 <> mintingValue)
    $ logError "Final balances are incorrect"
  -- logError $ "######SYMBOL: " <> show currSymbol <> "##### Ref: " <> show ref
  return (currSymbol, mintingValue) -- assetClass currSymbol "NFT"

testMintControlNFT :: Run ()
testMintControlNFT = do
  [u1, _, _, _] <- setupUsers
  (_, _) <- mintNFT u1

  wait $ days 20

testMintControlNFTTwice :: Run ()
testMintControlNFTTwice = do
  [u1, _, _, _] <- setupUsers
  utxos <- utxoAt u1
  let [(ref, out)] = utxos
      currSymbol = scriptCurrencySymbol (nftScript ref CM.letters)
      mintingValue = valueFromTokenNames currSymbol CM.letters
      tx = mintNFTTx ref out CM.letters mintingValue u1
  submitTx u1 tx
  submitTx u1 tx
  v1 <- valueAt u1
  unless (v1 == adaValue 1_000_000_000 <> mintingValue)
    $ logError "Final balances are incorrect"
