{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- FIXME: -- Do not use strict Data {-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

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
import Plutus.V1.Ledger.Value
  ( CurrencySymbol (CurrencySymbol),
    TokenName (unTokenName),
  )
import Plutus.V2.Ledger.Api
  ( BuiltinByteString,
    BuiltinData,
    Datum (Datum),
    OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
    ScriptContext (scriptContextTxInfo),
    TxInInfo (txInInfoResolved),
    TxInfo (txInfoMint),
    TxOut (txOutDatum, txOutValue),
    UnsafeFromData (unsafeFromBuiltinData),
    Validator,
    ValidatorHash,
    mkValidatorScript,
  )
import Plutus.V2.Ledger.Contexts
  ( findOwnInput,
    getContinuingOutputs,
  )
import PlutusTx
  ( CompiledCode,
    FromData (fromBuiltinData),
    applyCode,
    compile,
    liftCode,
    unstableMakeIsData,
  )
import PlutusTx.Prelude (Bool, Eq (..), Integer, Maybe (..), take, traceError, traceIfFalse, ($), (&&), (+), (.))
import Text.Printf (printf)
import Utilities (validatorHash', wrapValidator, writeValidatorToFile)
import Utilities.Utils
import Prelude (IO, Show (show))

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------
-- ####### DATUM
data TreeState = AdatagAdded | AdatagRemoved

unstableMakeIsData ''TreeState

-- Inline datum attached to the control NFT for carrying the state of the labeled tree.
-- The state contains:
-- 1. The last operation i.e. tag addedd or deleted.
-- 2. The nr. of operations since bootsrap (initial state). It always increases.
-- 3. The size of the tree i.e. the number of adatags. It can increse or decrease based on the operation.
-- 4. The added or deleted adatag.
-- 5. The root hash of the complete labeled binary tree.
-- 6. The associated minting policy that mints or burns adatags.
data ValidatorDatum = ValidatorDatum
  { vdOperationCount :: Integer, -- TODO: reconsider this. The number of operations (adding/deleting adatag) from bootstrap.
    vdAdatag :: BuiltinByteString, -- The username added or removed from the tree
    vdTreeState :: TreeState, -- The state of the Tree.
    vdTreeSize :: Integer, -- The size of a tree is the same as the number of adatags in the tree.
    vdTreeProof :: BuiltinByteString, -- The root hash of the tree, which proves the current state of the tree after a username has been added or deleted.
    vdMintingPolicy :: CurrencySymbol -- Corresponding adatag minting policy. It is used to avoid circular dependency between the validator and minting policy.
  }

unstableMakeIsData ''ValidatorDatum

-- Only inline datums are allowed.
{-# INLINEABLE parseValidatorDatum #-}
parseValidatorDatum :: OutputDatum -> Maybe ValidatorDatum
parseValidatorDatum o = case o of
  NoOutputDatum -> traceError "Found validator output but NoOutputDatum"
  OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d -- Inline datum
  OutputDatumHash _ -> traceError "Found validator output but no Inline datum"

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- Script param
type ControlNFT = CurrencySymbol

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
mkValidator :: ControlNFT -> ValidatorDatum -> () -> ScriptContext -> Bool
mkValidator cnft dat _ ctx =
  traceIfFalse "invalid output datum" hasValidDatum -- We assume that the correct initial state has been right fully implemented at bootstrap time.
    && traceIfFalse "token missing from output" hasValidMintingInfo
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Find the input currently being validated.
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "policy input missing"
      Just i -> txInInfoResolved i

    -- Get all the outputs that pay to the same script address we are currently spending from, if any.
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one policy output"

    -- Get a valid inline datum from a TxOut, if any
    ownDatum :: TxOut -> ValidatorDatum
    ownDatum txout = case parseValidatorDatum (txOutDatum txout) of
      Just d -> d
      Nothing -> traceError "invalid or non-inline datum"

    -- It checks that the minting policy Id is same in both datum &
    -- that the operations number is larger by 1 in the output datum
    hasValidDatum :: Bool
    hasValidDatum = do
      let dat' = ownDatum ownOutput
      vdMintingPolicy dat == vdMintingPolicy dat' && vdOperationCount dat + 1 == vdOperationCount dat'


    hasValidMintingInfo :: Bool
    hasValidMintingInfo = do
      -- Get only one token name of the currency symbol, if more exist then it trace error
      let itn = getOnlyTokenBySymbol (txOutValue ownInput) cnft
      -- Get only one token name of the currency symbol, if more exist then it trace error
      let otn = getOnlyTokenBySymbol (txOutValue ownOutput) cnft

      let odat = ownDatum ownOutput
      let adatag = vdAdatag odat
      let mintingPolicy = vdMintingPolicy odat

      let mintAdatag = getOnlyTokenBySymbol (txInfoMint info) mintingPolicy

      -- The input token name same as the output one it does not really necessary
      -- as we have ensured that there is only 1 input and 1 ouptut, meaning the normal Phase 1
      -- validation (accounting rules) will fail if this is not the case.
      -- The second one ensures that the adatag in the new tree state (adding/removing) is the same
      -- as in the minting policy.
      itn == otn && adatag == unTokenName mintAdatag

---------------------------------------------------------------------------------------------------
------------------------------------ COMPILE VALIDATOR --------------------------------------------

{-# INLINEABLE mkWrappedValidator #-}
mkWrappedValidator :: ControlNFT -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator . mkValidator

validator :: ControlNFT -> Validator
validator cnft =
  mkValidatorScript $
    $$(PlutusTx.compile [||mkWrappedValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode cnft

{-# INLINEABLE mkWrappedValidatorLucid #-}
--                            CS                                      redeemer       context
mkWrappedValidatorLucid :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidatorLucid cs = wrapValidator $ mkValidator cnft
  where
    cnft = CurrencySymbol $ unsafeFromBuiltinData cs

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(compile [||mkWrappedValidatorLucid||])

---------------------------------------------------------------------------------------------------
------------------------------------- SAVE VALIDATOR -------------------------------------------

-- savePolicyCode :: IO ()
-- savePolicyCode = writeCodeToFile "contracts/04-nft-validator.plutus" validatorCode

-- It only requires the conrol NFT currency symbol as the
-- minting policy ID will be privided in datum. Setup at bootstrap.
savePolicyScript :: CurrencySymbol -> IO ()
savePolicyScript cnft = do
  let
  writeValidatorToFile fp $ validator cnft
  where
    -- cnft = parseSymbol symbol
    fp = printf "contracts/04-control-nft-validator-%s.plutus" $ take 10 (show cnft)

---------------------------------------------------------------------------------------------------
---------------------------- HELPER FUNCTIONS FOR BOOTSTRAPPING -----------------------------------

parseSymbol :: String -> CurrencySymbol
parseSymbol s = CurrencySymbol $ fromString s

valHashBySymbol :: CurrencySymbol -> ValidatorHash
valHashBySymbol cnft = validatorHash' (validator cnft)

-- Keep it for now
-- valHash :: String -> ValidatorHash
-- valHash symbol = do
--  let
--  validatorHash' ( validator vp )
--  where
--    cnft = parseSymbol symbol
--    vp = cnft
--
-- scrAddress :: Address
-- scrAddress = scriptAddress validator