{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Contracts.AdatagMinting where

import Contracts.Validator
import qualified Data.ByteString.Char8 as BS8 (pack, unpack)
import Data.String (IsString (fromString))
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), TokenName (TokenName), assetClassValueOf, flattenValue, CurrencySymbol)
import Plutus.V2.Ledger.Api
  ( BuiltinData,
    MintingPolicy,
    OutputDatum (..),
    ScriptContext (scriptContextTxInfo),
    TxInInfo (txInInfoResolved),
    TxInfo (txInfoMint, txInfoReferenceInputs),
    TxOut (txOutAddress),
    UnsafeFromData (unsafeFromBuiltinData),
    Value,
    mkMintingPolicyScript,
    txOutDatum,
    unTokenName, TxOutRef (txOutRefId, txOutRefIdx), ValidatorHash,
  )
import Plutus.V2.Ledger.Api qualified as PlutusV2
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, scriptOutputsAt)
import PlutusTx
  ( CompiledCode,
    applyCode,
    compile,
    liftCode,
    makeLift,
    unstableMakeIsData,
  )
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString), BuiltinInteger)
import PlutusTx.Prelude
  ( Bool (False),
    Eq ((==)),
    Integer,
    Maybe (..),
    filter,
    takeByteString,
    traceError,
    traceIfFalse,
    ($),
    (&&),
    (.), head,
  )
import Utilities (isValidUsername, wrapPolicy, writeCodeToFile)
import Prelude (IO, Show (show), String)
import Utilities.Serialise
import Text.Printf
import Contracts.ControlNFTMinting
import Utilities.Conversions

---------------------------------------------------------------------------------------------------
------------------------------------ ON-CHAIN: VALIDATOR ------------------------------------------

-- | Node data structure represents a node of the Labeled Complete Binary Tree.
newtype Val = Val (BuiltinByteString, BuiltinByteString)
  -- data Val = Val (BuiltinByteString, BuiltinByteString)
  deriving (Show)

PlutusTx.unstableMakeIsData ''Val

data Tag = Tag BuiltinInteger Val
  deriving (Show)

PlutusTx.unstableMakeIsData ''Tag

data Node
  = Leaf BuiltinByteString
  | Node BuiltinInteger (BuiltinByteString, BuiltinByteString) Node Node
  deriving (Show)

PlutusTx.unstableMakeIsData ''Node

---------------------------

-- Minting policy parameters.
data MintParams = MintParams
  { -- | AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
    mpControlNFT :: CurrencySymbol, -- The control NFT that's carrying the state of the @adatag tree.
    mpValidator :: PlutusV2.ValidatorHash, -- ControlNFT validator hash.
    mpTimeDeposit :: PlutusV2.ValidatorHash, -- TimeDeposit validator hash. -- for checking time-lock deposits
    mpLockExpiry :: PlutusV2.POSIXTime, -- The POSIXTime until the Lock Time Deposit Feature is active
    --  (e.g. ~6-9 months form the @adatag bootstrapping).
    mpUserLockingPeriod :: PlutusV2.POSIXTime, -- The POSIXTime from the Lock Time Deposit is available for the user to claim.
    --  The user's `lockEndDate` in the datum, created by the minting script is currentTime + userLockingPeriod
    mpDepositBase :: BuiltinInteger, -- The user's deposit is calculated from deposit base and the lenght of the username.
    {-
      The formula of calculating the user's time lock deposit is `mpDepositBase * (maxLength - usernameLength + 1)`.
      Therefore, mpDepositBase = 50 means, that the value of the locked time deposit for a 1 length username is 750ADA.
      Example: 50 * ( 16 - 1 + 1) = 750. the 16 is the max length of an @adatag.
    -}
     mpAdaHandle         :: ValidatorHash -- For bypassing the time-lock output requirement when the user own's and $adahandle same with the @adatag being created
  }
  deriving (Prelude.Show)

makeLift ''MintParams

-- ####### REDEEMER
-- The users can mint new usernames or burn theirs.
-- An update action could be considered when the users allows changing it's data hash or similar.
-- Only if, when the Tag is modified to contain an additional 32-byte long data.
-- This, could be used for holding a public key hash or similar arbitrary data.
-- Note: Not implemented in this version of @adatag.
data MintAction = AddUser | DeleteUser

unstableMakeIsData ''MintAction

-- The users can only mint or burn usernames based on the redeemer.
data MintRedeemer = MintRedeemer
  { mrAction :: MintAction,
    mrUserName :: BuiltinByteString,
    mrUpdateLabel :: Val,
    mrAppendLabel :: Val,
    mrMinimalTree :: Node
  }

unstableMakeIsData ''MintRedeemer

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINEABLE mkPolicy #-}
mkPolicy :: MintParams -> MintRedeemer -> ScriptContext -> Bool
mkPolicy mp r ctx = case mrAction r of
  AddUser -> traceIfFalse "the username or cnft is invalid" checkUserName
  -- ##### Validator and control NFT related rules
  -- has valid control NFT datum
  --  traceIfFalse "has valid control NFT datums and redeemer" checValidatorDatum
  --  traceIfFalse "has valid control NFT datuma and redeemer" checValidatorDatum
  --
  -- traceError "Testing" -- IfFalse "minted amount must be exactly 1" checkNFTMint
  --  && traceIfFalse "invalid datum at timeDeposit output" checkTimeDepositDatum
  DeleteUser -> traceIfFalse "the username or cnft is invalid" checkUserName
  -- traceError "error" -- IfFalse "burning amount must be exactly -1" checkNFTBurn -- FIXME: &&
  --  traceIfFalse "owner's signature missing" checkColOwner &&
  -- traceIfFalse "Minting instead of burning!" checkNFTBurn
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    --------- MINTING-RELATED FUNCTIONS ------------

    -- Get amount and the name of the minted/burned adatag in this transaction
    -- It will fail later if the username in the redeemer and the datum are.
    mintedAmount :: Integer
    mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (ownCurrencySymbol ctx, TokenName $ mrUserName r))

    -- FIXME: -- Checks this to ensure no other tokens are minted
    -- checkMintedAmount :: Bool
    -- checkMintedAmount = case flattenValue (txInfoMint info) of
    --    [(_, tn'', amt)] -> tn'' == tn && amt == 1
    --    _                -> False

    -- Check that the amount of username is exactly one minted is positive
    checkNFTMint :: Bool
    checkNFTMint = mintedAmount == 1

    -- Check that the amount of stablecoins burned is negative
    checkNFTBurn :: Bool
    checkNFTBurn = mintedAmount == -1

    --------- VALIDATOR & CONTROL NFT FUNCTIONS ------------

    -- Parse the contorl NFT's input and output datum residing on validator script address.
    -- We do not check control NFT integrity as it's the Validator's responsibility.
    -- Though, we need the token name of the control NFT for allowing only valid usernames
    -- that are starts with the token name.
    --
    -- 1. First we do some sanity check on the output's datum (new state) and the username
    -- e.g. the username is valid and datum's username == redeemer's username.

    -- Get the controlNFT's output datum (value is validated in the validator script)
    stateOutput :: (OutputDatum, Value)
    stateOutput = case scriptOutputsAt (mpValidator mp) info of
      [(h, v)] -> (h, v)
      _ -> traceError "expected exactly one controlNFT output" -- It can happen as the minting scrip can run before the validator.

    -- Get the validator's output datum
    stateOutputDatum :: Maybe ValidatorDatum
    stateOutputDatum = parseValidatorDatum d
      where
        (d, _) = stateOutput

    -- Get the validators's output value
    stateOutputValue :: Value
    stateOutputValue = v
      where
        (_, v) = stateOutput

    -- Sanity checks
    -- It checks the followings:
    -- 1. the redeemer's username == the new state's username
    -- 2. the username is a valid @adatag
    -- 3. cNFT == the username's 1st character
    checkUserName :: Bool
    checkUserName = do
      case stateOutputDatum of
        Nothing -> False
        Just d -> do
          let cnft = getTokenName (mpControlNFT mp) stateOutputValue
          let u1 = mrUserName r
          let u2 = vdUserName d
          --
          u1 == u2 && isValidUsername u2 && takeByteString 1 u2 == unTokenName cnft -- Due to the short-circuit evaluation it's not an empty string now.

    --------- VALIDATOR & CONTROL NFT INPUTS ------------
    -- Get the validator's input
    validatorInput :: TxOut
    validatorInput = case validatorInputs of
      [o] -> o
      _ -> traceError "expected exactly one validator input"
      where
        validatorInputs :: [TxOut]
        validatorInputs =
          [ o
            | i <- txInfoReferenceInputs info,
              let o = txInInfoResolved i,
              txOutAddress o == scriptHashAddress (mpValidator mp)
          ]

    -- Get the control NFT's token name
    getTokenName :: CurrencySymbol -> Value -> TokenName
    getTokenName symbol v = do
      let xs = flattenValue v
      let filtered = filter (\(c, _, _) -> c == symbol) xs
      case filtered of
        [(_, tn, _)] -> tn
        _ -> traceError "expected exactly one token name"

    -- Get the collateral's input datum
    validatorInputDatum :: Maybe ValidatorDatum
    validatorInputDatum = parseValidatorDatum (txOutDatum validatorInput)

---------------------------------------------------------------------------------------------------
------------------------------ COMPILE AND SERIALIZE VALIDATOR ------------------------------------

{-# INLINEABLE mkWrappedPolicy #-}
mkWrappedPolicy :: MintParams -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicy = wrapPolicy . mkPolicy

policy :: MintParams -> MintingPolicy
policy np =
  mkMintingPolicyScript $
    $$(compile [||mkWrappedPolicy||])
      `applyCode` liftCode np

{-# INLINEABLE mkWrappedPolicyLucid #-}
--                     oracle ValHash   coll ValHash   minPercent      redeemer       context
mkWrappedPolicyLucid :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicyLucid tdv cv p = wrapPolicy $ mkPolicy mp
  where
    mp =
      MintParams
        { mpTimeDeposit = unsafeFromBuiltinData tdv,
          mpControlNFT = unsafeFromBuiltinData cv,
          mpValidator = unsafeFromBuiltinData cv,
          mpLockExpiry = unsafeFromBuiltinData cv,
          mpUserLockingPeriod = unsafeFromBuiltinData cv,
          mpDepositBase = unsafeFromBuiltinData cv,
          mpAdaHandle = unsafeFromBuiltinData cv
        }

policyCodeLucid :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
policyCodeLucid = $$(compile [||mkWrappedPolicyLucid||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveAdatagMintingCode :: Prelude.IO ()
saveAdatagMintingCode = writeCodeToFile "contracts/05-adatag-minting.plutus" policyCodeLucid

saveAdatagMintingPolicy :: TxOutRef -> [TokenName] -> IO ()
saveAdatagMintingPolicy oref tn =
  writePolicyToFile
    ( printf
        "contracts/05-adatag-minting-%s#%d-%s.plutus"
        (show $ txOutRefId oref)
        (txOutRefIdx oref)
        tn'
    )
    $ nftPolicy oref tn
  where
    tn' :: String
    tn' = case unTokenName $ head tn of
      (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs
