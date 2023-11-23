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

import Contracts.AdatagMinting
import Contracts.AlwaysFail
import Contracts.ControlNFTMinting
import Contracts.TimeDeposit
import Contracts.Validator
import Control.Monad (Monad (return), replicateM)
import Plutus.Model
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V2
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import PlutusTx.Prelude (AdditiveGroup (..), Bool, Eq ((==)), Integer, Semigroup ((<>)), filter, indexByteString, lengthOfByteString, ($), (+), (.))
import System.IO
import Test.Tasty (defaultMain, testGroup)
import Prelude (Bool (..))
import qualified Prelude as Haskell

main :: IO ()
main =
  defaultMain $
    testGroup
      "Testing @adatag's NFT minting logic"
      [ testGroup
          "Deploying and basic input/output tests"
          [ good "Must pass - deploy @adatag with defaults (365, 183, 20, 1750)" testBootstrapping, 
            --                                                                      vinp  tinp   validrange active deadline deposit
            bad  "Must fail - no state holder validator output"    $ testMintAdatag False  True   True        True   True    True,
            bad  "Must fail - no any validator output"             $ testMintAdatag False False   True        True   True    True,
            bad  "Must fail - active but no time-deposit output"   $ testMintAdatag  True False   True        True   True    True,
            good "Must pass - deactivated and no time-lock output" $ testMintAdatag  True False   True       False   True    True
          ],
        testGroup
          "Parameters and interval unit tests"                  --     vinp   tinp   validrange  active deadline deposit
          [ good "Must pass - all valid"                $ testMintAdatag True  True   True        True   True     True,
            bad  "Must fail - always interval ([-,+])"  $ testMintAdatag True  True  False        True   True     True,
            bad  "Must fail - invalid deadline"         $ testMintAdatag True  True   True        True   False    True,
            bad  "Must fail - invalid time-deposit"     $ testMintAdatag True  True   True        True   False   False,
            good "Must pass - deactivated"              $ testMintAdatag True  True   True       False   False   False
          ]
      ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors{-Trace-} (adaValue 10_000_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

mockTest :: Bool -> Run ()
mockTest b = case b of
  True -> logInfo "True..."
  False -> logError "False..."

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 4 $ newUser $ ada (Lovelace 1_000_000_000)

c2bs :: Integer -> BuiltinByteString
c2bs i = stringToBuiltinByteString $ Haskell.show i

createInitialStateDatum :: BuiltinByteString -> CurrencySymbol -> ValidatorDatum
createInitialStateDatum adatag mpsym = do
  let c = indexByteString adatag 0
      l = c - 1
      r = c + 1
      v = c2bs l <> c2bs r
  ValidatorDatum
    { vdOperationCount = 0,
      vdAdatag = "",
      vdTreeSize = 0,
      vdTreeState = InitialState,
      vdTreeProof = combineHash (hash v) $ combineHash (hash "") (hash ""),
      vdMintingPolicy = mpsym
    }

copyWith :: ValidatorDatum -> BuiltinByteString -> TreeState -> ValidatorDatum
copyWith (ValidatorDatum oc at _ s _ mp) nat ns = do
  let treesize = case ns of
        InitialState -> 0
        AdatagAdded -> s + 1
        AdatagRemoved -> s - 1

  ValidatorDatum
    { vdOperationCount = oc + 1,
      vdAdatag = nat,
      vdTreeState = InitialState,
      vdTreeSize = treesize,
      vdTreeProof = combineHash (hash $ nat <> at) $ combineHash (hash "") (hash ""),
      vdMintingPolicy = mp
    }

createTimeDepositDatum :: PubKeyHash -> POSIXTime -> TimeDepositDatum
createTimeDepositDatum pkh pt =
  TimeDepositDatum
    { ddBeneficiary = pkh,
      ddDeadline = pt
    }

createMintRedeemer :: BuiltinByteString -> MintAction -> MintRedeemer
createMintRedeemer at ma =
  MintRedeemer
    { mrAction = ma,
      mrAdatag = at,
      mrUpdateLabel = val at "a",
      mrAppendLabel = val at "b",
      mrMinimalTree = leaf
    }

---------------------------------------------------------------------------------------------------
-------------------------------------- @adatag VALIDATORS -----------------------------------------
-- Always Fail Validator
type AlwaysFailScript = TypedValidator () () -- No datum/redeemer

afvScript :: AlwaysFailScript
afvScript = TypedValidator $ toV2 alwaysFailValidator

-- Controlo NFT

cnftScript :: TxOutRef -> TypedPolicy ()
cnftScript ref = TypedPolicy . toV2 $ cnftPolicy ref letters

-- State Holder Validator
type ValidatorScript = TypedValidator ValidatorDatum () -- No redeemer

validatorScript :: ControlNFT -> ValidatorScript
validatorScript cnft = TypedValidator $ toV2 $ stateHolderValidator cnft

-- Time Deposit Validator
type TimeDepositScript = TypedValidator TimeDepositDatum ()

timeDepositParams :: PubKeyHash -> POSIXTime -> TimeDepositParams
timeDepositParams pkh time = do
  TimeDepositParams
    { dpCollector = pkh,
      dpCollectionTime = time
    }

timeDepositScript :: TimeDepositParams -> TimeDepositScript
timeDepositScript tp = TypedValidator $ toV2 $ timeDepositValidator tp

-- Adatag Minting Policy

type MintingScript = TypedPolicy MintRedeemer

adatagPolicyParams :: CurrencySymbol -> ValidatorHash -> ValidatorHash -> POSIXTime -> Integer -> Integer -> MintParams
adatagPolicyParams cnft shv tdv exp lp md =
  MintParams
    { mpControlNFT = cnft,
      mpStateHolderValidator = shv,
      mpTimeDepositValidator = tdv,
      mpUserDepositFeatureExpiry = exp,
      mpUserDepositLockingDays = lp,
      mpDepositBase = md
    }

mintingScript :: MintParams -> MintingScript
mintingScript mp = TypedPolicy . toV2 $ adatagPolicy mp

deadline :: POSIXTime
deadline = POSIXTime 20

---------------------------------------------------------------------------------------------------
-------------------------------------- @adatag UNIT TESTS -----------------------------------------
testBootstrapping :: Run ()
testBootstrapping = do
  (_, _, _, _, _) <- bootstrapAdatag 365 183 20 1_750

  logInfo "@adatag properly bootstrapped!"

-- Deploy Validator script
-- 1. CNFT minting and send to the developer.
-- 2. Set time-lock deposit - the developer will be the collector.
-- 3. Adatag Minting Policy setup
-- 4. Send cnfts to state holder validator's address -- it requires a datum with minting policy
-- 5. Send all three validator (state-holder, time-deposit, adatag-minting a ref addres (AFV)
--    > Note: currently loadRefScript does not support sending to individual addresses and it requires
--      datum, so cannot set for minting policies atm.
bootstrapAdatag :: Integer -> Integer -> Integer -> Integer -> Run (MintingScript, MintParams, ValidatorScript, TimeDepositScript, CurrencySymbol)
bootstrapAdatag collt expt deadl maxd = do
  --------------------------------------------------------------------------
  -- 1. CNFT minting and send to the developer.
  --------------------------------------------------------------------------
  developer <- newUser $ adaValue 26 -- 1 ada for each cnft
  utxos <- utxoAt developer

  let [(ref, out)] = utxos
      cnft = controlNFTCurrencySymbol ref
      shv = validatorScript cnft
      mintingValue = Haskell.mconcat [singleton cnft tn 1 | tn <- letters]

  -- mint cnft
  -- the received user has 26 ada and 26 cnfts
  submitTx developer $ mintCnftTx ref out mintingValue developer

  --------------------------------------------------------------------------
  -- 2. Set time-lock deposit - the developer will be the collector.
  --------------------------------------------------------------------------
  c <- currentTime
  let ct = getPOSIXTime $ days collt
      colltime = POSIXTime (getPOSIXTime c + ct)

      tdp = timeDepositParams developer colltime
      tdv = timeDepositScript tdp

      --------------------------------------------------------------------------
      -- 3. Adatag Minting Policy setup
      --------------------------------------------------------------------------
      le = getPOSIXTime $ days expt
      lockexpiry = POSIXTime (getPOSIXTime c + le)
      dl = deadl -- 20 -- deadline in days
      maxdeposit = maxd --  1_750
      mp = adatagPolicyParams cnft (validatorHash shv) (validatorHash tdv) lockexpiry dl maxdeposit
      amp = mintingScript mp

  --------------------------------------------------------------------------
  -- 4. Send cnfts to state holder validator's address -- it requires a datum with minting policy
  --------------------------------------------------------------------------

  v <- valueAt developer
  sp <- spend developer v

  -- send
  -- All init datum can be the same as no state check only sanity checks
  let dtx = deployCnftsTx sp shv cnft amp
  -- logInfo $ "Deploying CNFTs to state holder validator's address" <> Haskell.show dtx
  submitTx developer dtx
  --------------------------------------------------------------------------
  -- 5. Send all three validator (state-holder, time-deposit, adatag-minting a ref addres (AFV)
  -- This version of PSM does not support sending scripts to a ref addres, so we jsut use the scripts
  -- address until supports sending script to any reference address.
  -- But this mean that minting policies cannot be used as they do not have datums.
  --------------------------------------------------------------------------
  u1 <- newUser $ adaValue 2
  sp1 <- spend u1 $ adaValue 2

  let ltx = loadScriptsRefTx sp1 shv tdv --  amp
  -- logInfo $ "Deploying validators to reference address (mocking only)" <> Haskell.show ltx
  submitTx u1 ltx

  return (amp, mp, shv, tdv, cnft)
  where
    mintCnftTx ref out v pkh =
      Haskell.mconcat
        [ mintValue (cnftScript ref) () v,
          payToKey pkh $ v <> txOutValue out,
          spendPubKey ref
        ]
    loadScriptsRefTx sp vscr tscr =
      Haskell.mconcat
        [ userSpend sp,
          loadRefScript vscr $ adaValue 1,
          loadRefScript tscr $ adaValue 1
          -- loadRefScript mscr $ adaValue 1
        ]
    deployCnftsTx dsp scr curr mintpol =
      Haskell.mconcat
        [ userSpend dsp,
          Haskell.mconcat
            [ payToRef scr (InlineDatum $ createInitialStateDatum (unTokenName tn) (scriptCurrencySymbol mintpol)) (adaValue 1 <> singleton curr tn 1) | tn <- letters
            ]
        ]

-- Build transactions that will test the following cases.
-- 1. time-deposit: active, deadline: reached.
--    - inputs: control NFT
--    - ouptus: controlNFT, time-deposit,
--    - mintings: adatag.
-- 2. state holder validator:
-- 3. adatag minting policy:
testMintAdatag :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Run ()
-- valid validator output, valid deposit output, valid interval, active, valid deadlin and valid deposit
testMintAdatag vout  tout vint  act vdl vdep = do

  -- Bootstrap parameters.
  let collectionStart = 365
      depositExpiry = 183
      userDeadline = 20
      depositBase = 1_750

  (amp, mp, shv, tdv, cnftSymbol) <- bootstrapAdatag collectionStart depositExpiry userDeadline depositBase

  c <- currentTime


  let cnftToken = "i"
      adatag = "ilap"
      vdat = copyWith (createInitialStateDatum cnftToken (scriptCurrencySymbol amp)) adatag AdatagAdded

      -- Create deadline
      d = if vdl then 0 else -86_400_000 -- minus one day 
      t = getPOSIXTime $ days userDeadline
      dl = POSIXTime (getPOSIXTime c + t + d)

      -- Create deposit --
      v = if vdep then 0 else -1
      ld = getMinLockingDeposit (mpDepositBase mp) (lengthOfByteString adatag)  - v
      dep = if tout then ld else 0

  beneficiary <- newUser $ adaValue (dep + 10) -- deposit plus some for fees

  let tdat = createTimeDepositDatum beneficiary dl
-- logInfo $   "######## DAYS: \n" <> Haskell.show (days userDeadline) <>  "\n##### DEADLINE ######\n" <> Haskell.show dl <> "\n##### DL: " <> Haskell.show mp

  sp <- spend beneficiary $ singleton adaSymbol adaToken dep
  utxos <- utxoAt shv

  -- Find the required CNFT in utxos
  let [(ref, _)] =
        filter
          ( \(_, txOutTx) ->
              let flattenedValue = flattenValue $ txOutValue txOutTx
               in Haskell.any (\(_, tn, _) -> tn == TokenName cnftToken) flattenedValue
          )
          utxos

      mtr = createMintRedeemer adatag AddAdatag
      mval = singleton (scriptCurrencySymbol amp) (TokenName adatag) 1
      cnft = singleton cnftSymbol (TokenName cnftToken) 1
      mtx = mintAdatagTx beneficiary sp ref amp mtr mval shv (createInitialStateDatum adatag cnftSymbol) vdat cnft tdv tdat (adaValue dep) vout tout

  --logError $ "All Tn.\n" <> Haskell.show mtx  <> " \nmpControlNFT \n: " <> Haskell.show (mpControlNFT mp) <> "Value:\n" <> Haskell.show cnft-- Haskell.show mtx


  -- Wait till deactivation dat
  let waitfor = if act then POSIXTime 0 else days (depositExpiry + 250)
  wait waitfor

  range <- currentTimeInterval (POSIXTime (-100)) (POSIXTime 100)
  r1 <- validateIn range mtx
  r2 <- validateIn always mtx

  logInfo $  "@@@@@ Range: " <>  Haskell.show range <> "\nmpUserLockingExpiry (minting params): " <> Haskell.show mp <> "\n### WAITFOR\n" <> Haskell.show waitfor
  let tx = if vint then r1 else r2
  submitTx beneficiary tx

-- mintAdatagTx
--              user          us           ref         mpol             mred            adatag   vpol               vidat             vodat              cnft     tpol                 tdat               deposit
mintAdatagTx :: PubKeyHash -> UserSpend -> TxOutRef -> MintingScript -> MintRedeemer -> Value -> ValidatorScript -> ValidatorDatum -> ValidatorDatum -> Value -> TimeDepositScript -> TimeDepositDatum -> Value -> Bool -> Bool -> Tx
mintAdatagTx u us ref mpol mred adatag vpol vidat vodat cnft tpol tdat deposit hasv hast = 
  Haskell.mconcat
  [ mintValue mpol mred adatag,
    Haskell.mconcat [payToScript tpol (InlineDatum tdat) deposit | hast],
    Haskell.mconcat [spendScript vpol ref () vidat | hasv],
    Haskell.mconcat [payToScript vpol (InlineDatum vodat) (adaValue 1 <> cnft) | hasv],
    payToKey u adatag,
    userSpend us
  ]



mintAdatag :: Run ()
mintAdatag = do
  logInfo "Mint @adatag"

burnAdatag :: Run ()
burnAdatag = do
  logInfo "Burn @adatag"

{-
          mintValue mockMintPolicy () (singleton mintSymbol at 1),
          spendScript scr ref () idat,
          userSpend sp,
          payToScript scr (InlineDatum odat) (adaValue 1 <> singleton cnft "a" 1),
          payToKey pkh (adaValue 1 <> singleton mintSymbol at 1)
          -}