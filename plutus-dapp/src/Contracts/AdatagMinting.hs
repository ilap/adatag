{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-} -- FIXME: remove it on release
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# HLINT ignore "Use if" #-}
-- {-# HLINT ignore "Use if" #-}

module Contracts.AdatagMinting where

import Contracts.TimeDeposit
import Contracts.Validator
import Data.String (IsString (fromString))
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Interval hiding (singleton)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), CurrencySymbol, TokenName (TokenName), adaSymbol, adaToken, assetClassValueOf, valueOf)
import Plutus.V2.Ledger.Api
  ( BuiltinByteString,
    BuiltinData,
    MintingPolicy,
    OutputDatum (..),
    POSIXTime (..),
    POSIXTimeRange,
    ScriptContext (scriptContextTxInfo),
    TxInInfo (txInInfoResolved),
    TxInfo (txInfoMint),
    TxOut (txOutAddress),
    UnsafeFromData (unsafeFromBuiltinData),
    ValidatorHash (..),
    Value (..),
    mkMintingPolicyScript,
    txInfoInputs,
    txInfoValidRange,
    txOutDatum,
    unTokenName,
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
import PlutusTx.Builtins.Internal (BuiltinInteger)
import PlutusTx.Integer
import PlutusTx.Prelude
  ( Bool (False, True),
    Eq ((==)),
    Maybe (..),
    Ord (..),
    divide,
    lengthOfByteString,
    takeByteString,
    traceError,
    traceIfFalse,
    ($),
    (&&),
    (+),
    (-),
    (*), otherwise
  )

import Utilities (wrapPolicy)
import Utilities.Serialise
import Utilities.Utils
import Prelude (IO, Show)-- (show))



---------------------------------------------------------------------------------------------------
------------------------------------ LABELED TREE MOCKS -------------------------------------------
-- FIXME: These are vill be in the LabeledTree library

-- | Node data structure represents a node of the Labeled Complete Binary Tree.
newtype Val = Val (BuiltinByteString, BuiltinByteString)
  -- data Val = Val (BuiltinByteString, BuiltinByteString)
  deriving (Show)

PlutusTx.unstableMakeIsData ''Val

toVal :: BuiltinByteString -> BuiltinByteString -> Val
toVal bs1 bs2 = Val (bs1, bs2)

fromVal :: Val -> (BuiltinByteString, BuiltinByteString)
fromVal (Val (bs1, bs2)) = (bs1, bs2)

data Tag = Tag BuiltinInteger Val
  deriving (Show)

PlutusTx.unstableMakeIsData ''Tag

data Node
  = Leaf BuiltinByteString
  | Node BuiltinInteger (BuiltinByteString, BuiltinByteString) Node Node
  deriving (Show)

PlutusTx.unstableMakeIsData ''Node

---------------------------------------------------------------------------------------------------
------------------------------------ ON-CHAIN: VALIDATOR ------------------------------------------

-- Minting policy parameters.
data MintParams = MintParams
  { -- | AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
    mpControlNFT :: CurrencySymbol, -- The control NFT that's carrying the state of the @adatag tree.
    mpStateHolderValidator :: PlutusV2.ValidatorHash, -- ControlNFT validator hash the control NFT resides on.
    mpTimeDepositValidator :: PlutusV2.ValidatorHash, -- TimeDeposit validator hash. -- for checking time-lock deposits
    mpUserDepositFeatureExpiry :: PlutusV2.POSIXTime, -- The POSIXTime (in ms) until the Lock Time Deposit Feature is active
    --  (e.g. ~6-9 months form the @adatag bootstrapping).
    mpUserDepositLockingDays :: Integer, -- The users deposit locked in days, if the feature is active. Preferably 20 days.
    --  The user's `deadline` in the datum - created by the front-end is currentTime + userDepositLockingDays * dayInseconds
    -- Note: the script would fail if the user's lock end date time in the output datum is larger than the currentTime + 2 * userDepositLockingDays * 86400
    -- to prevent any accidental over locking.
    mpDepositBase :: BuiltinInteger -- The user's min deposit for locking an @adatag.
    {-
      The formula of calculating the user's time lock deposit is `floor (minPrice / 2 ^ (strLen - 1))` when it's > 5 otherwise 5ADA.
      Therefore, mpDepositBase = 1750 means, that locked value for a 1 length username is 1750ADA and for a 10 length one is 5.
      Note: We should have an different one when only the min 4 length adatags hav high time-lock deposits, the others the len > 5
      will only have 10 or 5 ADA.
    -}
    -- TODO: in later version mpAdaHandle :: ValidatorHash -- For bypassing the time-lock output requirement when the user own's and $adahandle same with the @adatag being created
  }
  deriving (Prelude.Show)

makeLift ''MintParams

------- REDEEMER
-- The users can mint new usernames or burn theirs.
-- An update action could be considered when the users allows changing it's data hash or similar.
-- Only if, when the Tag is modified to contain an additional 32-byte long data.
-- This, could be used for holding a public key hash or similar arbitrary data.
-- Note: Not implemented in this version of @adatag.
data MintAction = AddAdatag | DeleteAdatag
instance Eq MintAction where
  AddAdatag == AddAdatag = True
  DeleteAdatag == DeleteAdatag = True
  _ == _ = False
unstableMakeIsData ''MintAction

-- The users can only mint or burn usernames based on the redeemer.
data MintRedeemer = MintRedeemer
  { mrAction :: MintAction,
    mrAdatag :: BuiltinByteString,
    mrUpdateLabel :: Val,
    mrAppendLabel :: Val,
    mrMinimalTree :: Node
  }

unstableMakeIsData ''MintRedeemer

-- FIXME: -- These are just mock proover.

{-# INLINEABLE val #-}
val :: BuiltinByteString -> BuiltinByteString -> Val
val a b = Val (a, b)


{-# INLINEABLE leaf #-}
leaf :: Node
leaf = Leaf (fromString "")

{-# INLINEABLE buildTree #-}
buildTree :: BuiltinByteString -> (BuiltinByteString, BuiltinByteString) -> (BuiltinByteString, BuiltinByteString) -> Node -> Bool -> Node
buildTree _ _ _ _ _ = leaf

{-# INLINEABLE checkUpdate #-}
checkUpdate :: BuiltinByteString -> (BuiltinByteString, BuiltinByteString) -> (BuiltinByteString, BuiltinByteString) -> Node -> Bool -> Bool
checkUpdate x nu na u add = do
  -- add adatag when add is true
  let bt = buildTree x nu na u add
  True

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- The length of the adatag is already validated as 0 < x < 16
{-# INLINEABLE getMinLockingDeposit #-}
getMinLockingDeposit :: Integer -> Integer -> Integer
getMinLockingDeposit md i
  | i == 1  = md
  | i == 2  = divide md 2
  | i == 3  = divide md 4
  | i == 4  = divide md 8
  | i == 5  = divide md 16
  | otherwise  = 5


{-# INLINEABLE mkPolicy #-}
mkPolicy :: MintParams -> MintRedeemer -> ScriptContext -> Bool
mkPolicy mp red ctx = do
  let checkAdatagError = "invalid control nft or adatag"
  let checkActionError = "wrong redemer action or mint amount"
  -- let checkTreeState = "wrong redeemer submitted"
  let checkTimeLockDepositError = "time lock-deposit needs proper interval"

  case mrAction red of
    AddAdatag ->
      traceIfFalse checkActionError checkNFTMint
        && traceIfFalse checkAdatagError checkAdatag
        && traceIfFalse checkTimeLockDepositError checkTimeLockDeposit
        -- && traceIfFalse checkTreeState hasValidTreeState
    DeleteAdatag ->
      traceIfFalse checkActionError checkNFTBurn
        && traceIfFalse checkAdatagError checkAdatag

  where
    -- && traceIfFalse checkTreeState hasValidTreeState

    info :: TxInfo
    info = scriptContextTxInfo ctx

    --------- BASIC MINTING-RELATED FUNCTIONS ------------

    -- Get amount and the name of the minted/burned adatag in this transaction
    -- It ensured that adatag in the redeemer exist in the txInfoMint's value.
    -- But, does not validate whether ther is any other own token names are burned/minted.
    mintedAmount :: Integer
    mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (ownCurrencySymbol ctx, TokenName $ mrAdatag red))

    -- Check that the amount of adatag minted is exactly one and positive,
    -- and that the action in redeemer is AddTag
    checkNFTMint :: Bool
    checkNFTMint =  mintedAmount == 1 && mrAction red == AddAdatag

    -- Check that the amount of adatag burned is exactly one and negative
    -- and that the action in redeemer is DeleteTag
    checkNFTBurn :: Bool
    checkNFTBurn = mintedAmount == -1 && mrAction red == DeleteAdatag

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
    stateOutput = case scriptOutputsAt (mpStateHolderValidator mp) info of
      [(h, v)] -> (h, v)
      _ -> traceError "expected exactly one state-holder output" -- It can happen as the minting scrip can run before the validator.

    -- Get the validator's output datum
    stateOutputDatum :: ValidatorDatum
    stateOutputDatum = case parseValidatorDatum d of
      Nothing -> traceError "expected valid datum in validator output"
      Just d' -> d'
      where
        (d, _) = stateOutput

    -- Get the validators's output value
    stateOutputValue :: Value
    stateOutputValue = v
      where
        (_, v) = stateOutput

    -- Sanity checks
    -- It checks the followings:
    -- 1. the redeemer's adatag == the new state's adatag
    -- redeemer's adatag is already checked with checkNFTBurn & checkNFTMint functions
    -- 2. the applied adatag is a valid @adatag
    -- 3. cnft == the adatag's 1st character
    checkAdatag :: Bool
    checkAdatag = do
      let dat = stateOutputDatum
      -- In validator it's already checked that only 1 CNFT token in it's output value.
      let cnft = getOnlyTokenBySymbol stateOutputValue (mpControlNFT mp)
      let ra = mrAdatag red
      let da = vdAdatag dat
      --
      ra == da && isValidUsername da && takeByteString 1 da == unTokenName cnft -- Due to the short-circuit evaluation it's not an empty string now.

    ---------------- TIME LOCK VALIDATION ------------------
    -- No we're checking a valid Time-Lock deposit output, we can avoid checking when
    -- 1. the feature is deactivated approx 6 months after bootstrap
    timelockOutput :: (OutputDatum, Value)
    timelockOutput = case scriptOutputsAt (mpTimeDepositValidator mp) info of
      [(h, v)] -> (h, v)
      _ -> traceError "expected exactly one time-deposit output" -- It can happen as the minting scrip can run before the validator.

    -- Get the validator's output datum
    timelockOutputDatum :: TimeDepositDatum
    timelockOutputDatum = case parseTimeDepositDatum d of
      Nothing -> traceError "expected valid datum in time-lock validator output"
      Just d' -> d'
      where
        (d, _) = timelockOutput

    -- Get the validators's output value
    timelockOutputValue :: Value
    timelockOutputValue = v
      where
        (_, v) = timelockOutput

    deactivationReached :: Bool
    deactivationReached = contains (from $ mpUserDepositFeatureExpiry mp) $ txInfoValidRange info

    checkTimeLockDeposit :: Bool
    checkTimeLockDeposit = do
      
      -- Check valid POSIXTime interval first. 
      -- It's important that the user must set up a proper interval to use @adatag,
      -- as it is required to always check the deactivation time and a valid deadline when it's still active.
      let ct = case getUpperBoundTime $ txInfoValidRange info of -- get the current upper bound time in POSIXTime
                Just u -> u
                Nothing -> traceError "invalid upperbound in interval (probably PosInf)"

      case deactivationReached of
        True ->  True
        False -> validateTimeLockDeposit ct

    -- Convert a POSIXTimeRange into (POSIXTime,POSIXTime).
    getUpperBoundTime :: POSIXTimeRange -> Maybe POSIXTime
    getUpperBoundTime (Interval _ (UpperBound (Finite l) _)) = Just l
    getUpperBoundTime _ = Nothing

    -- The time-lock output must be more than mpUserDepositLockingDays in the future and
    -- its value must be at least the min lock-time deposit calculated from the length of the adatag.
    validateTimeLockDeposit :: POSIXTime -> Bool
    validateTimeLockDeposit ct = do

        -- Get the deadline from time-lock deposit's output datum. It must be at least `mpUserDepositLockingDays` days in the future
        let udl = ddDeadline timelockOutputDatum

  

            -- add 20 days - 3 hours in milliseconds. The user needs to build a valid transaction
            -- with a valid interval [-Infinite, valid upper bound]. 
            -- We assume that the valid upper bound  + 20 days - 3 hours is less than deadline.
            -- If not, then the user did not build a valid transaction.
            mdl = ct + POSIXTime (mpUserDepositLockingDays mp * 86400000 - 10800000) -- 20 days - 3 hours in ms.                      

            -- Check that that value of the time-lock deposit output has at least the required deposit
            tval = timelockOutputValue
            v = valueOf tval adaSymbol adaToken

            adatag = mrAdatag red
            d = getMinLockingDeposit (mpDepositBase mp) (lengthOfByteString adatag)

        mdl <= udl && v >= d

    --------- COMPLEX PROOF VALIDATION FUNCTIONS ------------
    -- From now we need to focus on the tree proofs of the updated tree state (adatag added or deleted)
    -- using only
    -- 1. the user's submitted values in the redeemer.
    -- 2. the vdTreeProof, and vdMintingPolicy fields of the old state (inpu datum of the validator), and
    -- 3. all the fields from the new state, the in output datum of the validator.
    --
    -- Meaning some sanity and tree proof checks of the updated tree state,
    -- which contains the following checks (' represents new state value):
    -- vdOperationCount' == vdOperationCount + 1
    -- vdAtatag' == is the adatag in the redeemer.
    -- vdTreeState' == AdatagAdded, when redemeer's action is AddAdatag, AdatagDeleted otherwise
    -- vdTreeSize' == vdTreSize +/- 1, depends on the redeemer's action
    -- vdTreeProof' == the result of the checkUpdate function using the parameters retrieved from the redeemer.
    -- vdTreeProof == same as above, as the checkUpdate returns with the calculated oldTreeProof and newTreeProof proofs.
    -- vdMintingPolicy' == vdMintingPolicy, this is a mandatory check in the Validator and an optional check here in the minting policy.
    checkStates :: ValidatorDatum -> ValidatorDatum -> Bool
    checkStates s s' =
      vdOperationCount s' == vdOperationCount s + 1
        && vdAdatag s' == mrAdatag red
        && vdMintingPolicy s' == vdMintingPolicy s -- Not required, as validator already checks it.
        && case vdTreeState s' of
          AdatagAdded -> vdTreeSize s + 1 == vdTreeSize s' && mrAction red == AddAdatag
          AdatagRemoved -> vdTreeSize s - 1 == vdTreeSize s' && mrAction red == DeleteAdatag

    -- Get the old state from validator's input
    -- Get the collateral's input
    validatorInput :: TxOut
    validatorInput = case validatorInputs of
      [o] -> o
      _ -> traceError "expected exactly one collateral input"
      where
        validatorInputs =
          [ o
            | i <- txInfoInputs info,
              let o = txInInfoResolved i,
              txOutAddress o == scriptHashAddress (mpStateHolderValidator mp)
          ]

    -- Get the collateral's input datum
    stateInputDatum :: ValidatorDatum
    stateInputDatum = case parseValidatorDatum (txOutDatum validatorInput) of
      Nothing -> traceError "old tree state cannot be found"
      Just d -> d

    -- FIXME: impelemt checkUpdate.
    hasValidTreeState :: Bool
    hasValidTreeState =
      do
        let state = stateInputDatum
        let state' = stateOutputDatum

        checkStates state state'
        && case mrAction red of
          AddAdatag -> checkUpdate (mrAdatag red) (fromVal $ mrUpdateLabel red) (fromVal $ mrAppendLabel red) (mrMinimalTree red) True
          DeleteAdatag -> checkUpdate (mrAdatag red) (fromVal $ mrUpdateLabel red) (fromVal $ mrAppendLabel red) (mrMinimalTree red) False

---------------------------------------------------------------------------------------------------
------------------------------ COMPILE AND SERIALIZE VALIDATOR ------------------------------------

{-# INLINEABLE mkWrappedPolicy #-}
--                  params        redeem         context
mkWrappedPolicy :: MintParams -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicy mp = wrapPolicy $ mkPolicy mp

adatagPolicy :: MintParams -> MintingPolicy
adatagPolicy mp =
  mkMintingPolicyScript $
    $$(compile [||mkWrappedPolicy||])
      `applyCode` liftCode mp

{-# INLINEABLE mkWrappedPolicyLucid #-}
--                     oracle ValHash   coll ValHash   minPercent      redeemer       context
mkWrappedPolicyLucid :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicyLucid cnft v tdv le ulp md = wrapPolicy $ mkPolicy mp
  where
    mp =
      MintParams
        { mpControlNFT = unsafeFromBuiltinData cnft,
          mpStateHolderValidator = unsafeFromBuiltinData v,
          mpTimeDepositValidator = unsafeFromBuiltinData tdv,
          mpUserDepositFeatureExpiry = unsafeFromBuiltinData le,
          mpUserDepositLockingDays = unsafeFromBuiltinData ulp,
          mpDepositBase = unsafeFromBuiltinData md
          -- mpAdaHandle = unsafeFromBuiltinData ah
        }

policyCodeLucid :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
policyCodeLucid = $$(compile [||mkWrappedPolicyLucid||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveAdatagMintingLucidCode :: IO ()
saveAdatagMintingLucidCode = writeCodeToFile "contracts/05-adatag-minting-lucid.plutus" policyCodeLucid

saveAdatagMintingPolicy :: CurrencySymbol -> ValidatorHash -> ValidatorHash -> POSIXTime -> Integer -> Integer -> IO ()
saveAdatagMintingPolicy cnft valh tdv exp lp md = writePolicyToFile "contracts/05-adatag-minting.plutus" $ adatagPolicy mp
  where
    mp =
      MintParams
        { mpControlNFT = cnft,
          mpStateHolderValidator = valh,
          mpTimeDepositValidator = tdv,
          mpUserDepositFeatureExpiry = exp,
          mpUserDepositLockingDays = lp, -- in days
          mpDepositBase = md
         --  mpAdaHandle = ah
        }

{-
      MintParams
        { mpControlNFT = CurrencySymbol "3e52d74291cae1976d8d1d547aa60485747018079c04423b72d61d78",
          mpStateHolderValidator = ValidatorHash "4e3cfec0374862ce3afea511509de01cb4be4484827be7ba797a5535",
          mpTimeDepositValidator = ValidatorHash "d9f85671159954a1c764aa63f859eebf4a753153754546f6d697e0ed",
          mpUserDepositFeatureExpiry = 1731325566000,
          mpUserDepositLockingDays = 1728000,
          mpDepositBase = 1750,
          mpAdaHandle = ValidatorHash "8d18d786e92776c824607fd8e193ec535c79dc61ea2405ddf3b09fe3"
        }
-}