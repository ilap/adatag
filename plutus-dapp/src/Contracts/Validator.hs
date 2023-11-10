{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- FIXME: -- Do not use strict Data {-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- This is a simple parameterized validator designed for handling control
NFTs, which store the state of a tree used for minting and burning usernames.

The minting policy, which is parameterised into this validator, ensures the integrity
of the control NFT's datum. The datum contains information about the current
state of the usernames tree, including the number of elements in the tree, a proof of
the tree, and the last element inserted into or removed from the tree.

This validator's primary function is to transfer (spend) the control NFT to its
own address only when a minting policy is present in the transaction.

The minting policy permits minting or burning usernames only when there is only
one control NFT in the transaction.

This design restricts the minting and burning of usernames to one operation at a time,
ensuring that the control NFT can only be moved from one of its addresses to another
of its addresses.
-}
module Contracts.Validator where

import Data.String (IsString (fromString), String)
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Value
  ( AssetClass (AssetClass),
    CurrencySymbol (CurrencySymbol),
    symbols,
    unCurrencySymbol,
  )
import Plutus.V2.Ledger.Api
  ( BuiltinByteString,
    BuiltinData,
    Datum (Datum),
    OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
    ScriptContext (scriptContextTxInfo),
    TxInInfo (txInInfoResolved),
    TxInfo,
    TxOut (txOutDatum, txOutValue),
    UnsafeFromData (unsafeFromBuiltinData),
    Validator,
    mkValidatorScript,
    txInfoInputs,
    txOutAddress,
    unCurrencySymbol, ValidatorHash,
  )
import Plutus.V2.Ledger.Contexts
  ( findDatum,
    findOwnInput,
    getContinuingOutputs,
  )
import PlutusTx
  ( CompiledCode,
    FromData (fromBuiltinData),
    applyCode,
    compile,
    liftCode,
    makeLift,
    unstableMakeIsData,
  )
import PlutusTx.Prelude
  ( Bool,
    Eq (..),
    Integer,
    Maybe (..),
    isJust,
    take,
    traceError,
    traceIfFalse,
    ($),
    (&&),
    (.),
  )
import Text.Printf (printf)
import Utilities (wrapValidator, writeCodeToFile, writeValidatorToFile,getTokenNameOfSymbol,symbolToValidatorHash)
import Prelude (IO, Show (show))

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------
-- ####### DATUM
data TreeState = UserAdded | UserRemoved | Initial

unstableMakeIsData ''TreeState

-- Inline datum attached to the control NFT for carrying the state of the labeled tree.
data ValidatorDatum = ValidatorDatum
  { vdUserAction :: TreeState, -- The state of the Tree.
    vdUserNameCount :: Integer, -- The number of usernames in the tree.
    vdUserName :: BuiltinByteString, -- The username added or removed from the tree
    vdTreeRootHash :: BuiltinByteString -- The tree root hash of the tree after the user is added or removed.
  }

unstableMakeIsData ''ValidatorDatum

{-# INLINEABLE parseValidatorDatum #-}
parseValidatorDatum :: OutputDatum -> TxInfo -> Maybe ValidatorDatum
parseValidatorDatum o info = case o of
  NoOutputDatum -> traceError "Found Validator output but NoOutputDatum"
  OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
  OutputDatumHash dh -> do
    Datum d <- findDatum dh info
    PlutusTx.fromBuiltinData d

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- ####### PARAMS
data ValidatorParams = ValidatorParams
  { -- | AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
    vpControlNFT :: CurrencySymbol,
    -- | ScriptHash of the minting policy
    vpMintingPolicy :: ValidatorHash
  }

PlutusTx.makeLift ''ValidatorParams

-- ####### REDEEMER
data ValidatorRedeemer = Update
  deriving (Prelude.Show)

unstableMakeIsData ''ValidatorRedeemer

{-
  Assumtpion: There are only 26 control NFTs, which cyrrency symbol is specified in the validator
  params, are residing on 26 UTxOs of the. Initial UTxOs contains the initial state of each
  username trees (Ta .. Tz)

  The validator must checks the following:
  1. its own NFT name (one of the "a".."z") being validated.
  2. some part of the new state (datum) in the output datum of the own NFT name being validated.
     - gets the username of the datum
  3. the minting info Value contains the minting policy id + username as assetname.

  this ensures only max 26 @adatag minting/burning can be actioned in a transaction.

  Optionaly we can have the following checks to take validations/checks from minting policy:
  - Only one mintingpolicy.username in the mintgin info value
  - the new state's (output's UTxO) username, proof, and username count (based on minting action) differs from the previous state
   in the input's UTxO.
-}
{-# INLINEABLE mkValidator #-}
mkValidator :: ValidatorParams -> ValidatorDatum -> ValidatorRedeemer -> ScriptContext -> Bool
mkValidator vp _ r ctx =
  case r of
    Update ->
      -- FIXME: -- It must check the followings:
      -- 1. input and output has exactly 1 control NFT token.
      -- 2. the token name in input and output must be the same (does not matter which one)
      -- 3. the value of the assetclas cnft token must be 1 (does not really matter as these are NFTs and
      -- created at bootstrap time, means value cannot be more than ones)
      -- 4. it checks whether it has a valid ouptut datum (it's not interested in its values only that it's esist and well formed)
      -- does not really matter, but good to have to fail faster.
      -- traceIfFalse "token missing from input" inputHasToken
      traceIfFalse "token missing from output" outputHasToken
        && traceIfFalse "invalid output datum" validOutputDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- We must ensure that there is no more than 1 inputs in the transaction
    -- findOwnInput (the input is being validated) is not suitable to this as the validator will validate every
    -- script inputs in the transactions and we have 26 control NFTs.
    -- Therefore, we must certain that only one input and one output presents in the
    -- transaction.
    controlNFTInput :: TxOut
    controlNFTInput = case controlNFTInputs of
      [o] -> o
      _ -> traceError "expected exactly one control NFT input"
      where
        controlNFTInputs =
          [ o
            | i <- txInfoInputs info,
              let o = txInInfoResolved i,
              txOutAddress o == scriptHashAddress (symbolToValidatorHash $ vpControlNFT vp)
          ]

    -- It ensures that the one control NFT is in our policy. This check is not necessary as
    -- the control NFTs are sent to the validator script address at the bootstrap time.
    -- Meaning no way of having control NFTs at different addresses, as this validator ensures
    -- that control NFTs are sent only to the validators script hash address.
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "policy input missing"
      Just i -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one policy output"

    -- Comparing input and output's control NFT's token name is not necessary,
    -- as the previous checks validated that only one input and output exist with control NFTs.
    -- Meaning the normal accounting check would fail
    tni = getTokenNameOfSymbol (txOutValue ownInput) (vpControlNFT vp)
    -- tno = getTokenNameBySymbol (vpControlNFT vp) (txOutValue ownOutput)

    -- nftAssetClass = AssetClass (vpControlNFT vp, tni)

    -- Check that the policy input contains the NFT's currency symbol.
    -- FIXME: -- We are not interested in token names as the minting policy ensures
    -- the integrity of the control NFT's token name.
    -- inputHasToken :: Bool
    --inputHasToken = assetClassValueOf (txOutValue ownInput) (oNFT oracle) == 1

    hasSymbol :: CurrencySymbol
    hasSymbol = case symbols $ txOutValue ownInput of -- ownInput
      [c] -> c
      _ -> traceError "expected exactly one policy output"

    -- Check that the policy output contains the control NFT.
    outputHasToken :: Bool
    outputHasToken = hasSymbol == vpControlNFT vp

    --  Only checks that the policy output contains a valid datum.
    validOutputDatum :: Bool
    validOutputDatum = isJust $ parseValidatorDatum (txOutDatum ownOutput) info

---------------------------------------------------------------------------------------------------
------------------------------------ COMPILE VALIDATOR --------------------------------------------

{-# INLINEABLE mkWrappedValidator #-}
mkWrappedValidator :: ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator . mkValidator

validator :: ValidatorParams -> Validator
validator vp =
  mkValidatorScript $
    $$(PlutusTx.compile [||mkWrappedValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode vp

{-# INLINEABLE mkWrappedValidatorLucid #-}
--                            CS              TN          sh              redeemer       context
mkWrappedValidatorLucid :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidatorLucid cs _ sh = wrapValidator $ mkValidator op
  where
    op =
      ValidatorParams
        { vpControlNFT = CurrencySymbol $ unsafeFromBuiltinData cs,
          vpMintingPolicy = unsafeFromBuiltinData sh
        }

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(compile [||mkWrappedValidatorLucid||])

---------------------------------------------------------------------------------------------------
------------------------------------- SAVE VALIDATOR -------------------------------------------

savePolicyCode :: IO ()
savePolicyCode = writeCodeToFile "assets/05-nft-validator.plutus" validatorCode

-- It requires the conrol NFT currency symbol and the
-- minting validator hash.
savePolicyScript :: String -> ValidatorHash -> IO ()
savePolicyScript symbol sh = do
  let
  writeValidatorToFile fp $ validator op
  where
    s = parseSymbol symbol
    op =
      ValidatorParams
        { vpControlNFT = s,
          vpMintingPolicy = sh
        }
    fp = printf "assets/05-control-nft-validator-%s-%s.plutus" (take 3 (show sh)) $ take 3 (show s)

parseSymbol :: String -> CurrencySymbol
parseSymbol s = CurrencySymbol $ fromString s