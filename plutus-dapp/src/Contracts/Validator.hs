{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}


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

import PlutusLedgerApi.V2.Contexts
  ( findOwnInput,
    getContinuingOutputs,
  )
import PlutusTx
  ( CompiledCode,
    FromData (fromBuiltinData),
    compile,
    liftCode,
    unstableMakeIsData,
  )
import PlutusTx.Prelude
    ( Eq(..),
      Integer,
      Maybe(..),
      traceError,
      traceIfFalse,
      ($),
      (&&),
      (+),
      (.),
      (>=),
      takeByteString,
      appendByteString,
      sha2_256,
      Bool )
import Utilities -- (validatorHash', wrapValidator, writeValidatorToFile)
import Prelude (Show (show))
import qualified Prelude as Haskell
import qualified Data.ByteString.Base16  as Haskell.Base16
import qualified Data.Text  as Haskell.Text
import qualified Data.Text.Encoding  as Haskell.Text.Encoding
import qualified PlutusTx
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2
---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------
-- ####### DATUM
-- | A type for representing hash digests.
newtype Hash = Hash BuiltinByteString
  deriving (Haskell.Eq)
unstableMakeIsData ''Hash
instance Eq Hash where
  (==) :: Hash -> Hash -> Bool
  Hash h == Hash h' = h == h'

instance Haskell.Show Hash where
  show (Hash bs) =
    Haskell.Text.unpack
      . Haskell.Text.Encoding.decodeUtf8
      . Haskell.Base16.encode
      . fromBuiltin
      . takeByteString 4
      $ bs

{-# INLINEABLE hash #-}
hash :: BuiltinByteString -> Hash
hash = Hash . sha2_256

{-# INLINEABLE combineHash #-}
combineHash :: Hash -> Hash -> Hash
combineHash (Hash h) (Hash h') = hash (appendByteString h h')

data TreeState = AdatagAdded | AdatagRemoved | InitialState
  deriving (Prelude.Show)
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
    vdTreeProof :: Hash, -- BuiltinByteString, -- The root hash of the tree, which proves the current state of the tree after a username has been added or deleted.
    vdMintingPolicy :: CurrencySymbol -- Corresponding adatag minting policy. It is used to avoid circular dependency between the validator and minting policy.
  } -- TODO: Only for testing, removed it from releases
  | UnitDatum ()
  | EmptyDatum {}
  | WrongDatum { mock :: Integer }
  deriving (Prelude.Show)

unstableMakeIsData ''ValidatorDatum

-- Only inline datums are allowed.
{-# INLINEABLE parseValidatorDatum #-}
parseValidatorDatum :: OutputDatum -> Maybe ValidatorDatum
parseValidatorDatum o = case o of
  NoOutputDatum -> traceError "Found validator output but NoOutputDatum"
  OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d-- Inline datum
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
{-# INLINEABLE stateHolderTypedValidator #-}
stateHolderTypedValidator :: ControlNFT -> ValidatorDatum -> () -> ScriptContext -> Bool
stateHolderTypedValidator cnft dat _ ctx =
    -- We assume that the system is properly bootstrapped
    traceIfFalse "invalid output datum" hasValidDatum
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
    -- TODO: This is just for unit tests
    ownDatum :: TxOut -> ValidatorDatum
    ownDatum txout = case parseValidatorDatum (txOutDatum txout) of
      Just (ValidatorDatum a b c d e f)   -> ValidatorDatum a b c d e f
      Just _ -> traceError "TEMPORARY: invalid datums"
      Nothing -> traceError "invalid or non-inline datum"

    -- It checks that the minting policy Ids are same in both datums &
    -- that the operations number is larger by 1 in the output datum.
    -- It also validates that the TreeSize newer be a negative number.
    hasValidDatum :: Bool
    hasValidDatum = do
      let dat' = ownDatum ownOutput
      -- it validates that
      vdMintingPolicy dat == vdMintingPolicy dat' && vdOperationCount dat + 1 == vdOperationCount dat' && vdTreeSize dat' >= 0


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

      -- It is not necessary to check that the input token name is the same as the output one,
      -- because we have ensured that there is only one input and one output.
      -- This means that the normal Phase 1 validation (accounting rules) will fail if the input
      -- token name is not the same as the output one. The second rule ensures that the data tag
      -- in the new tree state (adding/removing) is the same as in the minting policy.
      traceIfFalse "the output token name altered" (itn == otn) && traceIfFalse "minted token name is different" (adatag == unTokenName mintAdatag)

---------------------------------------------------------------------------------------------------
------------------------------------ COMPILE VALIDATOR --------------------------------------------

{-# INLINEABLE stateHolderUntypedValidator #-}
stateHolderUntypedValidator :: ControlNFT -> BuiltinData -> BuiltinData -> BuiltinData -> ()
stateHolderUntypedValidator = wrapValidator . stateHolderTypedValidator

stateHolderValidator :: ControlNFT -> PlutusTx.CompiledCode ( BuiltinData -> BuiltinData ->  BuiltinData -> ())
stateHolderValidator cnft =  $$(PlutusTx.compile [||stateHolderUntypedValidator||])
     `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 cnft

{-




{-# INLINEABLE mkWrappedValidator #-}
mkWrappedValidator :: ControlNFT -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator . stateHolderTypedValidator

stateHolderValidator :: ControlNFT -> Validator
stateHolderValidator cnft =
  stateHolderTypedValidatorScript $
    $$(PlutusTx.compile [||mkWrappedValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode cnft

{-# INLINEABLE mkWrappedValidatorLucid #-}
--                            CS                                      redeemer       context
mkWrappedValidatorLucid :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidatorLucid cs = wrapValidator $ stateHolderTypedValidator cnft
  where
    cnft = CurrencySymbol $ unsafeFromBuiltinData cs

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(compile [||mkWrappedValidatorLucid||])

-}
---------------------------------------------------------------------------------------------------
------------------------------------- SAVE VALIDATOR -------------------------------------------
{-
-- savePolicyCode :: IO ()
-- savePolicyCode = writeCodeToFile "contracts/04-nft-validator.plutus" validatorCode

-- It only requires the conrol NFT currency symbol as the
-- minting policy ID will be privided in datum. Setup at bootstrap.
savePolicyScript :: CurrencySymbol -> IO ()
savePolicyScript cnft = do
  let
  writeValidatorToFile fp $ stateHolderValidator cnft
  where
    -- cnft = parseSymbol symbol
    fp = printf "contracts/04-control-nft-validator-%s.plutus" $ take 10 (show cnft)

---------------------------------------------------------------------------------------------------
---------------------------- HELPER FUNCTIONS FOR BOOTSTRAPPING -----------------------------------

parseSymbol :: String -> CurrencySymbol
parseSymbol s = CurrencySymbol $ fromString s

valHashBySymbol :: CurrencySymbol -> ValidatorHash
valHashBySymbol cnft = validatorHash' (stateHolderValidator cnft)

-- Keep it for now
-- valHash :: String -> ValidatorHash
-- valHash symbol = do
--  let
--  validatorHash' ( stateHolderValidator vp )
--  where
--    cnft = parseSymbol symbol
--    vp = cnft
--
-- scrAddress :: Address
-- scrAddress = scriptAddress stateHolderValidator
-}