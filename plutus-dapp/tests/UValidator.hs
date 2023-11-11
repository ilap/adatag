{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import qualified Contracts.AdatagMinting as AM
import qualified Contracts.ControlNFTMinting as CM
import qualified Contracts.TimeDeposit as TD
import qualified Contracts.Validator as TV
import Control.Monad (Monad (return), replicateM, unless, void)
import Plutus.Model
import Plutus.Model.Fork.Ledger.Slot (Slot)
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol, TokenName (TokenName), assetClass, assetClassValue, symbols)
import Plutus.V2.Ledger.Api
  ( PubKeyHash,
    ScriptHash,
    TokenName,
    TxOut (txOutValue),
    TxOutRef,
    Value (..),
    singleton,
  )
import PlutusTx.Prelude (Eq ((==)), Integer, Maybe (Just, Nothing), Semigroup ((<>)), encodeUtf8, foldMap, ($), (.))
import System.IO
import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude (mconcat, show)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------
main :: IO ()
main = defaultMain $ do
  testGroup
    "Testing validator with some sensible values"
    [ good "Testing minting control NFTs      " testMintControlNFT
    -- ,good "Deploying the Validator works      " testDeployValidator
    --  , xxx "Accepts only 1 input & ouput       " xxx
    --  , xxx "" xxx
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 4 $ newUser $ ada (Lovelace 1_000_000_000)

type TagValidator = TypedValidator CurrencySymbol

mintingScript :: AM.MintParams -> TypedPolicy AM.MintRedeemer
mintingScript = TypedPolicy . toV2 . AM.policy

-- Time Deposit validator

type TimeDepositScript = TypedValidator TD.TimeDepositDatum ()

tdScript :: TimeDepositScript
tdScript = TypedValidator $ toV2 TD.validator

---------------------------------------------------------------------------------------------------
------------------------------------- CONTROL NFT FUNCTIONS --------------------------------------------
-- NFT Minting Policy's script
nftScript :: Plutus.V2.Ledger.Api.TxOutRef -> [Plutus.V2.Ledger.Api.TokenName] -> TypedPolicy ()
nftScript ref tn = TypedPolicy . toV2 $ CM.nftPolicy ref tn

mintNFTTx :: Plutus.V2.Ledger.Api.TxOutRef -> Plutus.V2.Ledger.Api.TxOut -> [Plutus.V2.Ledger.Api.TokenName] -> Plutus.V2.Ledger.Api.Value -> Plutus.V2.Ledger.Api.PubKeyHash -> Tx
mintNFTTx ref out tn val pkh =
  mconcat
    [ mintValue (nftScript ref tn) () val,
      payToKey pkh $ val <> Plutus.V2.Ledger.Api.txOutValue out,
      spendPubKey ref
    ]

-- Create a Plutus Value from a list of token names.
createValueFromTokenNames :: CurrencySymbol -> [Plutus.V2.Ledger.Api.TokenName] -> Plutus.V2.Ledger.Api.Value
createValueFromTokenNames cs = foldMap (\tn -> Plutus.V2.Ledger.Api.singleton cs tn 1)

mintNFT :: Plutus.V2.Ledger.Api.PubKeyHash -> Run Plutus.V2.Ledger.Api.Value
mintNFT u = do
  utxos <- utxoAt u
  let [(ref, out)] = utxos
      currSymbol = scriptCurrencySymbol (nftScript ref CM.letters)
      mintingValue = createValueFromTokenNames currSymbol CM.letters
  submitTx u $ mintNFTTx ref out CM.letters mintingValue u
  v1 <- valueAt u
  unless (v1 == adaValue 1_000_000_000 <> mintingValue) $
    logError "Final balances are incorrect"
  -- logError $ show mintingValue
  return mintingValue

testMintControlNFT :: Run ()
testMintControlNFT = do
  [u1, _, _, _] <- setupUsers
  void $ mintNFT u1

--------------------------------------VALIDATOR FUNCTIONS --------------------------------------------

-- Validator's script
validatorScript :: TV.ControlNFT -> TagValidator
validatorScript validator = TypedValidator . toV2 $ TV.validator validator

deployValidatorTx :: UserSpend -> TV.ControlNFT -> () -> Value -> Tx
deployValidatorTx sp op r val =
  mconcat
    [ userSpend sp,
      payToScript (validatorScript op) (InlineDatum 1) (adaValue 1 <> val)
    ]

nftTokenName :: TokenName
nftTokenName = TokenName $ encodeUtf8 "a"

deployValidator :: PubKeyHash -> () -> Run (TagValidator, AssetClass)
deployValidator u or = do
  -- Mint NFT
  nftAC <- mintNFT u
  -- Get users utxo to parameterize the control NFT validator
  utxos <- utxoAt u
  let [(ref, out)] = utxos
  let cn = Plutus.Model.validatorHash $ nftScript ref CM.letters

  -- cnftSymbols ....
  let s = case symbols nftAC of
        [o] -> o
  -- _   -> logError "expected exactly one oracle input"
  logError $ "AAAAAAAAAAA" <> show cn
  let a = assetClass s nftTokenName
  let nftV = assetClassValue a 1
  sp <- spend u $ adaValue 1 <> nftV
  -------
  let tvParams = cn
  --    oracleVH     = validatorHash' $ Oracle.validator oracleParams
  --    tdVH = validatorHash' TD.validator

  --    stablecoinMP = stableCoinScript $ Minting.MintParams tvVH tdVH
  ------

  -- let mintingVH = validatorHash' $ Oracle.validator op
  -- stableCoinScript $ Minting.MintParams oracleVH collateralVH 150
  let validator = cn
      validatorTx = deployValidatorTx sp validator or nftV
  submitTx u validatorTx
  return (validatorScript validator, a)

testDeployValidator :: Run ()
testDeployValidator = do
  [u1, _, _, _] <- setupUsers
  -- Deploy Validator
  (ov, _) <- deployValidator u1 ()
  logError "EXIT"
  -- Check that the validator deployed correctly
  [(ref, _)] <- utxoAt ov
  dat <- datumAt ref :: Run (Maybe Integer)
  case dat of
    Just r -> logError "Some datum exists" -- unless (r == 25) $ logError "Datum doesn't match!"
    _ -> logError "asdfsdf" --Nothing -- logError "Validator is not deployed correctly: Could not find datum"

{-
updateValidatorTx :: TV.ValidatorParams -> TV.Rate -> TV.Rate -> TxOutRef -> Value -> Tx
updateValidatorTx op last new oRef nft =
  mconcat
    [ spendScript (validatorScript op) oRef TV.Update last
    , payToScript (validatorScript op) (InlineDatum new) (adaValue 1 <> nft)
    ]

updateValidator :: PubKeyHash -> TV.Rate ->  TV.Rate -> TagValidator -> AssetClass -> Run ()
updateValidator u last new ov nftAC = do
  [(ref,_)] <- utxoAt ov
  let validator = TV.ValidatorParams nftAC u
      validatorTx = updateValidatorTx validator last new ref (assetClassValue nftAC 1)
  signed <- signTx u validatorTx
  submitTx u signed
  --return $ validatorScript validator

testUpdateValidator :: Run ()
testUpdateValidator = do
  [u1,_,_,_] <- setupUsers
  -- Deploy Validator
  (ov, ac) <- deployValidator u1 25
  -- Update Validator
  updateValidator u1 25 26 ov ac
  -- Check that the validator updated correctly
  [(ref,_)] <- utxoAt ov
  dat <- datumAt ref :: Run (Maybe TV.Rate)
  case dat of
    Just r -> unless (r == 26) $ logError "Datum doesn't match!"
    _ -> logError "Validator is not deployed correctly: Could not find datum"

testUpdateValidatorNoMintingPolicy :: Run ()
testUpdateValidatorNoMintingPolicy = do
  [u1,u2,_,_] <- setupUsers
  -- Deploy Validator
  (ov, ac) <- deployValidator u1 25
  -- Update Validator
  ov' <-  updateValidatorNMP u2 u1 25 26 ov ac
  -- Check that the validator updated correctly
  [(ref,_)] <- utxoAt ov'
  dat <- datumAt ref :: Run (Maybe TV.Rate)
  case dat of
    Just r -> unless (r == 26) $ logError "Datum doesn't match!"
    _ -> logError "Validator is not deployed correctly: Could not find datum"

updateValidatorNMP :: PubKeyHash -> PubKeyHash -> TV.Rate ->  TV.Rate -> TagValidator -> AssetClass -> Run TagValidator
updateValidatorNMP signer u last new ov nftAC = do
  [(ref,_)] <- utxoAt ov
  let validator = TV.ValidatorParams nftAC u
      validatorTx = updateValidatorTx validator last new ref (assetClassValue nftAC 1)
  signed <- signTx signer validatorTx
  submitTx signer validatorTx
  return $ validatorScript validator
-}
