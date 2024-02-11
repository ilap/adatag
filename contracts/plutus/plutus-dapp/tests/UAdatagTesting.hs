{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use underscore" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Adatag.AdatagMinting
import Adatag.ControlNFTMinting
import Adatag.StateHolder
import Adatag.TimeDeposit
import Control.Monad (Monad (return), replicateM)
import Plutus.Model
import Plutus.Model.V2 (mkTypedPolicy)
import Plutus.Model.Validator.V2 (mkTypedValidator)
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V2
import PlutusTx.Prelude (
  AdditiveGroup (..),
  Bool,
  Eq ((==)),
  Integer,
  Semigroup ((<>)),
  filter,
  lengthOfByteString,
  ($),
  (+),
  (.),
 )
import System.IO
import Test.Tasty (defaultMain, testGroup)
import Prelude (Bool (..), Show (show))
import Prelude qualified as Haskell
import IntegriTree (ProofTree (..), Hash, emptyHash, Val(..), rootHash')



main :: IO ()
main =
  defaultMain
    $ testGroup
      "Testing @adatag's NFT minting logic"
      [ testGroup
          "Deploying and basic input/output tests"
          [ good "Must pass - deploy @adatag with defaults (365, 183, 20, 1750)" testBootstrapping
          , --                                                                      vinp  tinp   validrange active deadline deposit
            bad "Must fail - no state holder validator output" $ testMintAdatag False True True True True True
          , bad "Must fail - no any validator output" $ testMintAdatag False False True True True True
          , bad "Must fail - active but no time-deposit output" $ testMintAdatag True False True True True True
          , good "Must pass - deactivated and no time-lock output" $ testMintAdatag True False True False True True
          ]
      , testGroup
          "Parameters and interval unit tests" --     vinp   tinp   validrange  active deadline deposit
          [ good "Must pass - all valid" $ testMintAdatag True True True True True True
          , bad "Must fail - always interval ([-,+])" $ testMintAdatag True True False True True True
          , bad "Must fail - invalid deadline" $ testMintAdatag True True True True False True
          , bad "Must fail - invalid time-deposit" $ testMintAdatag True True True True False False
          , good "Must pass - deactivated" $ testMintAdatag True True True False False False
          ]
      , testGroup
          "Labeled Tree unit tests" --     vinp   tinp   validrange  active deadline deposit
          [ good "Must pass - all valid" $ testMintAdatag True True True False False False

          ]
      ]
  where
    bad msg = good msg . mustFail
    good = testNoErrorsTrace (adaValue 10_000_000_000) defaultBabbage


---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------
pl = ProofLeaf emptyHash

initialTree :: Val -> ProofTree
-- initialTree v = ProofNode v (ProofLeaf emptyHash) (ProofLeaf emptyHash)
initialTree v = ProofNode (Val 2 "ib15" "ib16") pl (ProofNode (Val 2 "ib14" "ib15") (ProofNode (Val 5 "ib13" "ib14") pl (ProofNode (Val 8 "ib12" "ib13") (ProofNode (Val 17 "ib11" "ib12") pl (ProofNode (Val 32 "ib10" "ib11") (ProofNode (Val 65 "ib9" "ib10") pl (ProofNode (Val 128 "ib8" "ib9") (ProofNode (Val 257 "ib7" "ib8") pl (ProofNode (Val 512 "ib6" "ib7") (ProofNode (Val 1025 "ib5" "ib6") pl (ProofNode (Val 2048 "ib4" "ib5") (ProofNode (Val 4097 "ib3" "ib4") pl (ProofNode (Val 8192 "ib2" "ib3") (ProofNode (Val 16385 "ib1" "ib2") pl (ProofNode (Val 32768 "ib0" "ib1") (ProofNode v pl pl) pl)) pl)) pl)) pl)) pl)) pl)) pl)) pl)

addedTree :: Val -> Val -> ProofTree
--addedTree v1 v2 = ProofNode v1
--              (ProofNode v2 (ProofLeaf emptyHash) (ProofLeaf emptyHash)) 
--              (ProofLeaf emptyHash)
addedTree v1 v2 = ProofNode (Val 2 "ib15" "ib16") pl (ProofNode (Val 2 "ib14" "ib15") (ProofNode (Val 5 "ib13" "ib14") pl (ProofNode (Val 8 "ib12" "ib13") (ProofNode (Val 17 "ib11" "ib12") pl (ProofNode (Val 32 "ib10" "ib11") (ProofNode (Val 65 "ib9" "ib10") pl (ProofNode (Val 128 "ib8" "ib9") (ProofNode (Val 257 "ib7" "ib8") pl (ProofNode (Val 512 "ib6" "ib7") (ProofNode (Val 1_025 "ib5" "ib6") pl (ProofNode (Val 2_048 "ib4" "ib5") (ProofNode (Val 4_097 "ib3" "ib4") pl (ProofNode (Val 8_192 "ib2" "ib3") (ProofNode (Val 16_385 "ib1" "ib2") pl (ProofNode (Val 32_768 "ib0" "ib1") (ProofNode v1 (ProofNode v2 pl pl) pl) pl)) pl)) pl)) pl)) pl)) pl)) pl)) pl)
n = 15 :: Integer

tsize = 2 Haskell.^ (n+1)

tsize' = 2 Haskell.* tsize

cnftToken = "i"
adatag = "ilapilapilapila"

initVal = Val tsize "h" "j"

updateVal = Val tsize "h" adatag
appendVal = Val tsize "h" adatag
leafVal = Val tsize' adatag "j"

treeState = rootHash' $ initialTree initVal

treeState' = rootHash' $ addedTree updateVal leafVal

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

mockTest :: Bool -> Run ()
mockTest b = case b of
  True -> logInfo "True..."
  False -> logError "False..."

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 4 $ newUser $ ada (Lovelace 1_000_000_000)

createInitialStateDatum :: CurrencySymbol -> Hash -> ValidatorDatum
createInitialStateDatum mpsym h =
  ValidatorDatum
    { vdOperationCount = 0
    , vdAdatag = ""
    , vdTreeSize = 0
    , vdTreeState = InitialState
    , vdTreeProof = h
    , vdMintingPolicy = mpsym
    }

copyWith :: ValidatorDatum -> BuiltinByteString -> TreeState -> Hash -> ValidatorDatum
copyWith (ValidatorDatum oc _ _ s _ mp) nat ns root = do
  let treesize = case ns of
        InitialState -> 0
        AdatagAdded -> s + 1
        AdatagRemoved -> s - 1

  ValidatorDatum
    { vdOperationCount = oc + 1
    , vdAdatag = nat
    , vdTreeState = ns
    , vdTreeSize = treesize
    , vdTreeProof = root
    , vdMintingPolicy = mp
    }

createTimeDepositDatum :: PubKeyHash -> POSIXTime -> TimeDepositDatum
createTimeDepositDatum pkh pt =
  TimeDepositDatum
    { ddBeneficiary = pkh
    , ddDeadline = pt
    }

createMintRedeemer ::  MintAction -> BuiltinByteString -> Val -> Val -> ProofTree -> MintRedeemer
createMintRedeemer ma at uv av p =
  MintRedeemer
    { mrAction = ma
    , mrAdatag = at
    , mrUpdateVal = uv
    , mrAppendVal = av
    , mrProof = p
    }

---------------------------------------------------------------------------------------------------
-------------------------------------- @adatag VALIDATORS -----------------------------------------
-- Controlo NFT
cnftScript :: TxOutRef -> TypedPolicy ()
cnftScript ref = mkTypedPolicy $ cnftPolicy ref letters

-- State Holder validator
type ValidatorScript = TypedValidator ValidatorDatum () -- No redeemer

validatorScript :: ControlNFT -> ValidatorScript
validatorScript cnft = mkTypedValidator $ stateHolderValidator cnft

-- Time Deposit validator
type TimeDepositScript = TypedValidator TimeDepositDatum ()

timeDepositParams :: PubKeyHash -> POSIXTime -> TimeDepositParams
timeDepositParams pkh time = do
  TimeDepositParams
    { dpCollector = pkh
    , dpCollectionTime = time
    }

timeDepositScript :: TimeDepositParams -> TimeDepositScript
timeDepositScript tp = mkTypedValidator $ timeDepositValidator tp

-- Adatag Minting Policy

type MintingScript = TypedPolicy MintRedeemer

adatagPolicyParams :: CurrencySymbol -> ScriptHash -> ScriptHash -> POSIXTime -> Integer -> Integer -> MintParams
adatagPolicyParams cnft shv tdv exp lp md =
  MintParams
    { mpControlNFT = cnft
    , mpStateHolderValidator = shv
    , mpTimeDepositValidator = tdv
    , mpUserDepositFeatureExpiry = exp
    , mpUserDepositLockingDays = lp
    , mpDepositBase = md
    }

mintingScript :: MintParams -> MintingScript
mintingScript mp = mkTypedPolicy $ adatagMintingScript mp

deadline :: POSIXTime
deadline = POSIXTime 20

---------------------------------------------------------------------------------------------------
-------------------------------------- @adatag UNIT TESTS -----------------------------------------
testBootstrapping :: Run ()
testBootstrapping = do
  (_, _, _, _, _) <- bootstrapAdatag 365 183 20 1_750

  logInfo "@adatag is properly bootstrapped!"

-- Deploy StateHolder script
-- 1. CNFT minting and send to the developer.
-- 2. Set time-lock deposit - the developer will be the collector.
-- 3. Adatag Minting Policy setup
-- 4. Send cnfts to state holder validators's address -- it requires a datum with minting policy
-- 5. Send all three validators (state-holder, time-deposit, adatag-minting a ref addres (AFV)
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
      cnft = scriptCurrencySymbol $ cnftScript ref
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
      mp = adatagPolicyParams cnft (scriptHash shv) (scriptHash tdv) lockexpiry dl maxdeposit
      amp = mintingScript mp

  --------------------------------------------------------------------------
  -- 4. Send cnfts to state holder validator's address -- it requires a datum with minting policy
  --------------------------------------------------------------------------

  v <- valueAt developer
  sp <- spend developer v

  -- send
  -- All init datum can be the same as no state check only sanity checks
  let dtx = deployCnftsTx sp shv cnft amp

  submitTx developer dtx
  --------------------------------------------------------------------------
  -- TODO: Make it to use reference scripts
  -- 5. Send all three validators (state-holder, time-deposit, adatag-minting a ref addres (AFV)
  -- This version of PSM does not support sending scripts to a ref addres, so we jsut use the scripts
  -- address until supports sending script to any reference address.
  -- But this mean that minting policies cannot be used as they do not have datums.
  --------------------------------------------------------------------------

  return (amp, mp, shv, tdv, cnft)
  where
    mintCnftTx ref out v pkh =
      Haskell.mconcat
        [ mintValue (cnftScript ref) () v
        , payToKey pkh $ v <> txOutValue out
        , spendPubKey ref
        ]
    deployCnftsTx dsp scr curr mintpol =
      Haskell.mconcat
        [ userSpend dsp
        , Haskell.mconcat
            [ payToScript scr (InlineDatum $ createInitialStateDatum (scriptCurrencySymbol mintpol) treeState) (adaValue 1 <> singleton curr tn 1) | tn <- letters
            ]
        ]

-- Build transactions that will test the following cases.
-- 1. time-deposit: active, deadline: reached.
--    - inputs: authorization Token
--    - ouptus: controlNFT, time-deposit,
--    - mintings: adatag.
-- 2. state holder validator:
-- 3. adatag minting policy:
testMintAdatag :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Run ()
-- valid state-holder output, valid deposit output, valid interval, active, valid deadlin and valid deposit
testMintAdatag vout tout vint act vdl vdep = do
  -- Bootstrap parameters.
  let collectionStart = 365
      depositExpiry = 183
      userDeadline = 20
      depositBase = 1_750

  (amp, mp, shv, tdv, cnftSymbol) <- bootstrapAdatag collectionStart depositExpiry userDeadline depositBase

  c <- currentTime

  let vidat = createInitialStateDatum (scriptCurrencySymbol amp) treeState
      vodat = copyWith vidat adatag AdatagAdded treeState'
      -- Create deadline
      d = if vdl then 0 else -86_400_000 -- minus one day
      t = getPOSIXTime $ days userDeadline
      dl = POSIXTime (getPOSIXTime c + t + d)

      -- Create deposit --
      v = if vdep then 0 else -1
      ld = getMinLockingDeposit (mpDepositBase mp) (lengthOfByteString adatag) - v
      dep = if tout then ld else 0

  logInfo $ "################## VIDAT: " Haskell.++ Prelude.show vidat
  logInfo $ "################## VODAT: " Haskell.++ Prelude.show vodat

  beneficiary <- newUser $ adaValue (dep + 10) -- deposit plus some for fees
  let tdat = createTimeDepositDatum beneficiary dl

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
      -- uv = Val 1 "h" "j"
      -- proof = ProofNode uv (ProofLeaf emptyHash) (ProofLeaf emptyHash)
      mtr = createMintRedeemer AddAdatag adatag initVal initVal $ initialTree initVal
      mval = singleton (scriptCurrencySymbol amp) (TokenName adatag) 1
      cnft = singleton cnftSymbol (TokenName cnftToken) 1
      mtx = mintAdatagTx beneficiary sp ref amp mtr mval shv vidat vodat cnft tdv tdat (adaValue dep) vout tout

  -- Wait till feature deactivation
      waitfor = if act then POSIXTime 0 else days (depositExpiry + 250)
  logInfo $ "################## MTR: " Haskell.++ Prelude.show mtr
    
  wait waitfor

  range <- currentTimeInterval (POSIXTime (-100)) (POSIXTime 100)
  r1 <- validateIn range mtx
  r2 <- validateIn always mtx

  let tx = if vint then r1 else r2
  submitTx beneficiary tx

-- mintAdatagTx
--              user          us           ref         mpol             mred            adatag   vpol               vidat             vodat              cnft     tpol                 tdat               deposit
mintAdatagTx :: PubKeyHash -> UserSpend -> TxOutRef -> MintingScript -> MintRedeemer -> Value -> ValidatorScript -> ValidatorDatum -> ValidatorDatum -> Value -> TimeDepositScript -> TimeDepositDatum -> Value -> Bool -> Bool -> Tx
mintAdatagTx u us ref mpol mred at vpol vidat vodat cnft tpol tdat deposit hasv hast =
  Haskell.mconcat
    [ mintValue mpol mred at
    , Haskell.mconcat [payToScript tpol (InlineDatum tdat) deposit | hast]
    , Haskell.mconcat [spendScript vpol ref () vidat | hasv]
    , Haskell.mconcat [payToScript vpol (InlineDatum vodat) (adaValue 1 <> cnft) | hasv]
    , payToKey u at
    , userSpend us
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
