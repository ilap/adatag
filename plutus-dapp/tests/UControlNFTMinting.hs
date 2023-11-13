{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import qualified Contracts.ControlNFTMinting as CM
import Control.Monad (Monad (return), replicateM, unless, void)
import Plutus.Model
import Plutus.V1.Ledger.Value (CurrencySymbol)
import Plutus.V2.Ledger.Api (PubKeyHash, TokenName, TxOut (txOutValue), TxOutRef, Value (..), singleton)
import PlutusTx.Prelude (Eq ((==)), Semigroup ((<>)), foldMap, ($), (.))
import System.IO
import Test.Tasty (defaultMain, testGroup)
import Prelude (mconcat)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Testing validator with some sensible values"
      [ good "Minting Control NFTs works       " testMintControlNFT,
        bad "Minting the same NFTs twice fails" testMintControlNFTTwice
      ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 4 $ newUser $ ada (Lovelace 1_000_000_000)

-- NFT Minting Policy's script
nftScript :: TxOutRef -> [TokenName] -> TypedPolicy ()
nftScript ref tn = TypedPolicy . toV2 $ CM.nftPolicy ref tn

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING MINTING CONTROL NFT -----------------------------------------

mintNFTTx :: TxOutRef -> TxOut -> [TokenName] -> Value -> PubKeyHash -> Tx
mintNFTTx ref out tn val pkh =
  mconcat
    [ mintValue (nftScript ref tn) () val,
      payToKey pkh $ val <> txOutValue out,
      spendPubKey ref
    ]

-- Create a Plutus Value from a list of token names.
valueFromTokenNames :: CurrencySymbol -> [TokenName] -> Value
valueFromTokenNames cs = foldMap (\tn -> singleton cs tn 1)

mintNFT :: PubKeyHash -> Run Value
mintNFT u = do
  utxos <- utxoAt u
  let [(ref, out)] = utxos
      currSymbol = scriptCurrencySymbol (nftScript ref CM.letters)
      mintingValue = valueFromTokenNames currSymbol CM.letters
  submitTx u $ mintNFTTx ref out CM.letters mintingValue u
  v1 <- valueAt u
  unless (v1 == adaValue 1_000_000_000 <> mintingValue) $
    logError "Final balances are incorrect"
  return mintingValue -- assetClass currSymbol "NFT"

testMintControlNFT :: Run ()
testMintControlNFT = do
  [u1, _, _, _] <- setupUsers
  void $ mintNFT u1

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
  unless (v1 == adaValue 1_000_000_000 <> mintingValue) $
    logError "Final balances are incorrect"
