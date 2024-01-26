{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Adatag.AdatagMinting where

import Adatag.StateHolder
import Adatag.TimeDeposit
import Adatag.Utils
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Interval hiding (singleton)
import PlutusLedgerApi.V1.Value (
  AssetClass (AssetClass),
  CurrencySymbol,
  TokenName (TokenName),
  adaSymbol,
  adaToken,
  assetClassValueOf,
  valueOf,
 )
import PlutusLedgerApi.V2 (
  BuiltinByteString,
  BuiltinData,
  OutputDatum (..),
  POSIXTime (..),
  POSIXTimeRange,
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoMint),
  TxOut (txOutAddress),
  Value (..),
  txInfoInputs,
  txInfoValidRange,
  txOutDatum,
  unTokenName,
 )
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusLedgerApi.V2.Contexts (ownCurrencySymbol)
import PlutusTx (
  CompiledCode,
  compile,
  liftCode,
  makeLift,
  unsafeApplyCode,
  unstableMakeIsData,
 )
import PlutusTx.Builtins.Internal (BuiltinInteger)
import PlutusTx.Integer
import PlutusTx.Prelude (
  Bool (False, True),
  Eq ((==)),
  Maybe (..),
  Ord (..),
  divide,
  lengthOfByteString,
  otherwise,
  takeByteString,
  traceError,
  traceIfFalse,
  ($),
  (&&),
  (*),
  (+),
  (-),
 )
import Utilities (wrapPolicy)
import Utilities.Serialise
import Prelude (IO)
import qualified IntegriTree.Proofs as LT
import qualified Prelude as Haskell
import IntegriTree.Val
import IntegriTree.Proofs


---------------------------------------------------------------------------------------------------
--------------------------------------- ERROR TABLE  ----------------------------------------------
{-
  let checkAdatagError = 			"LT1" 	"invalid control nft or adatag"
  let checkActionError = 			"LT2"	"wrong redemer action or mint amount"
  let checkTreeState =        "LT3" "wrong redeemer submitted"
  let checkTimeLockDepositError = 	"TL4"	"time lock-deposit needs proper interval"
	  "LT5"	"expected exactly one state-holder output" -- It can happen as the minting scrip can run before the StateHolder.
    "LT6" "expected valid datum in StateHolder output"
     "LT7" expected exactly one time-deposit output" -- It can happen as the minting scrip can run before the StateHolder.
     "LT8" "expected valid datum in time-lock StateHolder output"
     "LT9" "invalid upperbound in interval (probably PosInf)"
     "LTa" "expected exactly one collateral input"
     "LTb" "old tree state cannot be found"
	   "LTc" "old tree state cannot be found"
-}

---------------------------------------------------------------------------------------------------
------------------------------------ ON-CHAIN: StateHolder ------------------------------------------

-- Minting policy parameters.
data MintParams = MintParams
  { mpControlNFT :: CurrencySymbol -- The control NFT that's carrying the state of the @adatag tree.

  -- ^ AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
  , mpStateHolderValidator :: PlutusV2.ScriptHash -- PlutusV2.PlutusScriptV2, -- ControlNFT StateHolder hash the control NFT resides on.
  , mpTimeDepositValidator :: PlutusV2.ScriptHash -- PlutusV2.PlutusScriptV2, -- TimeDeposit StateHolder hash. -- for checking time-lock deposits
  , mpUserDepositFeatureExpiry :: PlutusV2.POSIXTime -- The POSIXTime (in ms) until the Lock Time Deposit Feature is active
  --  (e.g. ~6-9 months form the @adatag bootstrapping).
  , mpUserDepositLockingDays :: Integer -- The users deposit locked in days, if the feature is active. Preferably 20 days.
  --  The user's `deadline` in the datum - created by the front-end is currentTime + userDepositLockingDays * dayInseconds
  -- Note: the script would fail if the user's lock end date time in the output datum is larger than the currentTime + 2 * userDepositLockingDays * 86400
  -- to prevent any accidental over locking.
  , mpDepositBase :: BuiltinInteger -- The user's min deposit for locking an @adatag.
  {-
    The formula of calculating the user's time lock deposit is `floor (minPrice / 2 ^ (strLen - 1))` when it's > 5 otherwise 5ADA.
    Therefore, mpDepositBase = 1750 means, that locked value for a 1 length username is 1750ADA and for a 10 length one is 5.
    Note: We should have an different one when only the min 4 length adatags hav high time-lock deposits, the others the len > 5
    will only have 10 or 5 ADA.
  -}
  -- TODO: in later version mpAdaHandle :: ValidatorHash -- For bypassing the time-lock output requirement when the user own's and $adahandle same with the @adatag being created
  }
  deriving (Haskell.Show)

PlutusTx.makeLift ''MintParams

------- REDEEMER
-- The users can mint new usernames or burn theirs.
-- An update action could be considered when the users allows changing it's data hash or similar.
-- Only if, when the Tag is modified to contain an additional 32-byte long data.
-- This, could be used for holding a public key hash or similar arbitrary data.
-- Note: Not implemented in this version of @adatag.
data MintAction = AddAdatag | DeleteAdatag
  deriving (Haskell.Show)

instance Eq MintAction where
  AddAdatag == AddAdatag = True
  DeleteAdatag == DeleteAdatag = True
  _ == _ = False

unstableMakeIsData ''MintAction

-- The users can only mint or burn usernames based on the redeemer.
data MintRedeemer = MintRedeemer
  { mrAction :: MintAction
  , mrAdatag :: BuiltinByteString
  , mrUpdateVal :: Val
  , mrAppendVal :: Val
  , mrProof :: ProofTree
  }
  deriving (Haskell.Show)

unstableMakeIsData ''MintRedeemer

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / StateHolder ------------------------------------------

-- The length of the adatag is already validated as 0 < x < 16
{-# INLINABLE getMinLockingDeposit #-}
getMinLockingDeposit :: Integer -> Integer -> Integer
getMinLockingDeposit md i
  | i == 1 = md -- ~1750
  | i == 2 = divide md 2 -- ~875
  | i == 3 = divide md 4 -- ~437
  | i == 4 = divide md 8 -- ~218
  | i == 5 = divide md 16 -- ~109
  | otherwise = 5

{-# INLINABLE scriptOutputsAt #-}
scriptOutputsAt :: PlutusV2.ScriptHash -> TxInfo -> [(PlutusV2.OutputDatum, Value)]
scriptOutputsAt h p =
  [(dh, v) | PlutusV2.TxOut {txOutAddress = PlutusV2.Address (PlutusV2.ScriptCredential s) _, PlutusV2.txOutValue = v, txOutDatum = dh} <- PlutusV2.txInfoOutputs p, s == h]

{-# INLINABLE mintingTypedPolicy #-}
mintingTypedPolicy :: MintParams -> MintRedeemer -> ScriptContext -> Bool
mintingTypedPolicy mp red ctx = do
  let checkAdatagError = "LT1"
  let checkActionError = "LT2"
  let checkTreeState = "LT3"
  let checkTimeLockDepositError = "LT4"

  case mrAction red of
    AddAdatag ->
      traceIfFalse checkActionError checkNFTMint
        && traceIfFalse checkAdatagError checkAdatag
        && traceIfFalse checkTimeLockDepositError checkTimeLockDeposit
        && traceIfFalse checkTreeState hasValidTreeState
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
    checkNFTMint = mintedAmount == 1 && mrAction red == AddAdatag

    -- Check that the amount of adatag burned is exactly one and negative
    -- and that the action in redeemer is DeleteTag
    checkNFTBurn :: Bool
    checkNFTBurn = mintedAmount == -1 && mrAction red == DeleteAdatag

    --------- StateHolder & CONTROL NFT FUNCTIONS ------------

    -- Parse the contorl NFT's input and output datum residing on StateHolder script address.
    -- We do not check control NFT integrity as it's the StateHolder's responsibility.
    -- Though, we need the token name of the control NFT for allowing only valid usernames
    -- that are starts with the token name.
    --
    -- 1. First we do some sanity check on the output's datum (new state) and the username
    -- e.g. the username is valid and datum's username == redeemer's username.

    -- Get the controlNFT's output datum (value is validated in the StateHolder script)
    stateOutput :: (OutputDatum, Value)
    stateOutput = case scriptOutputsAt (mpStateHolderValidator mp) info of
      [(h, v)] -> (h, v)
      _ -> traceError "LT5" -- It can happen as the minting scrip can run before the StateHolder.

    -- Get the StateHolder's output datum
    stateOutputDatum :: ValidatorDatum
    stateOutputDatum = case parseValidatorDatum d of
      Nothing -> traceError "LT6"
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
    -- 3. minting == the adatag's 1st character
    checkAdatag :: Bool
    checkAdatag = do
      let dat = stateOutputDatum
      -- In StateHolder it's already checked that only 1 minting token in it's output value.
      let minting = getOnlyTokenBySymbol stateOutputValue (mpControlNFT mp)
      let ra = mrAdatag red
      let da = vdAdatag dat
      --
      ra == da && isValidUsername da && takeByteString 1 da == unTokenName minting -- Due to the short-circuit evaluation it's not an empty string now.

    ---------------- TIME LOCK VALIDATION ------------------
    -- No we're checking a valid Time-Lock deposit output, we can avoid checking when
    -- 1. the feature is deactivated approx 6 months after bootstrap
    timelockOutput :: (OutputDatum, Value)
    timelockOutput = case scriptOutputsAt (mpTimeDepositValidator mp) info of
      [(h, v)] -> (h, v)
      _ -> traceError "LT7" -- It can happen as the minting scrip can run before the StateHolder.

    -- Get the StateHolder's output datum
    timelockOutputDatum :: TimeDepositDatum
    timelockOutputDatum = case parseTimeDepositDatum d of
      Nothing -> traceError "LT8"
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
            Nothing -> traceError "LT9"

      (if deactivationReached then True else validateTimeLockDeposit ct)

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
    -- 2. the vdTreeProof, and vdMintingPolicy fields of the old state (inpu datum of the StateHolder), and
    -- 3. all the fields from the new state, the in output datum of the StateHolder.
    --
    -- Meaning some sanity and tree proof checks of the updated tree state,
    -- which contains the following checks (' represents new state value):
    -- vdOperationCount' == vdOperationCount + 1
    -- vdAtatag' == is the adatag in the redeemer.
    -- vdTreeState' == AdatagAdded, when redemeer's action is AddAdatag, AdatagDeleted otherwise
    -- vdTreeSize' == vdTreSize +/- 1, depends on the redeemer's action
    -- vdTreeProof' == the result of the checkUpdate function using the parameters retrieved from the redeemer.
    -- vdTreeProof == same as above, as the checkUpdate returns with the calculated oldTreeProof and newTreeProof proofs.
    -- vdMintingPolicy' == vdMintingPolicy, this is a mandatory check in the StateHolder and an optional check here in the minting policy.
    checkStates :: ValidatorDatum -> ValidatorDatum -> Bool
    checkStates s s' =
          vdOperationCount s' == vdOperationCount s + 1
            && vdAdatag s' == mrAdatag red
            && vdMintingPolicy s' == vdMintingPolicy s
            && vdMintingPolicy s' == ownCurrencySymbol ctx
            && case (mrAction red, vdTreeState s') of
                (AddAdatag, AdatagAdded)    -> vdTreeSize s + 1 == vdTreeSize s'
                (DeleteAdatag, AdatagRemoved) -> vdTreeSize s - 1 == vdTreeSize s'
                _                           -> False
    -- Get the old state from StateHolder's input
    -- Get the collateral's input
    validatorInput :: PlutusV2.TxOut
    validatorInput = case validatorInputs of
      [o] -> o
      _ -> traceError "LTa"
      where
        validatorInputs =
          [ o
          | i <- txInfoInputs info
          , let o = txInInfoResolved i
          , txOutAddress o == scriptHashAddress (mpStateHolderValidator mp)
          ]

    -- Get the collateral's input datum
    stateInputDatum :: ValidatorDatum
    stateInputDatum = case parseValidatorDatum (txOutDatum validatorInput) of
      Nothing -> traceError "LTb"
      Just d -> d

   -- FIXME: impelemt checkUpdate.
    hasValidTreeState :: Bool
    hasValidTreeState =
      let state = stateInputDatum
          state' = stateOutputDatum
          checkStatesValid = checkStates state state'
      in
        checkStatesValid && case mrAction red of
          AddAdatag -> LT.checkAppend (mrAdatag red) (vdOperationCount state') (vdTreeProof state) (vdTreeProof state') (mrUpdateVal red) (mrAppendVal red) (mrProof red)

          -- FIXME: Implement Delete
          DeleteAdatag -> traceError "LTc"

---------------------------------------------------------------------------------------------------
------------------------------ COMPILE AND SERIALIZE StateHolder ------------------------------------

{-# INLINABLE mintingUntypedPolicy #-}
mintingUntypedPolicy :: MintParams -> BuiltinData -> BuiltinData -> ()
mintingUntypedPolicy mp = wrapPolicy $ mintingTypedPolicy mp

adatagMintingScript :: MintParams -> CompiledCode (BuiltinData -> BuiltinData -> ())
adatagMintingScript mp =
  $$(PlutusTx.compile [||mintingUntypedPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 mp

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveAdatagMintingPolicy :: CurrencySymbol -> PlutusV2.ScriptHash -> PlutusV2.ScriptHash -> POSIXTime -> Integer -> Integer -> IO ()
saveAdatagMintingPolicy cnft valh tdv exp ld db = writeCodeToFile "contracts/05-adatag-minting.plutus" $ adatagMintingScript mp
  where
    mp =
      MintParams
        { mpControlNFT = cnft
        , mpStateHolderValidator = valh
        , mpTimeDepositValidator = tdv
        , mpUserDepositFeatureExpiry = exp
        , mpUserDepositLockingDays = ld -- in days
        , mpDepositBase = db
        --  mpAdaHandle = ah
        }
