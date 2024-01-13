{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Adatag.ControlNFTMinting qualified as CNFT
import Adatag.StateHolder
import Control.Monad (Monad (return))
import Plutus.Model.V2
import PlutusLedgerApi.V1.Value (
  CurrencySymbol (..),
  TokenName (unTokenName),
 )
import PlutusLedgerApi.V2 (
  BuiltinByteString,
  ScriptContext,
  TokenName (TokenName),
  TxOut (txOutValue),
  TxOutRef,
  Value,
  singleton,
 )
import PlutusTx qualified
import PlutusTx.Builtins (Integer)
import PlutusTx.Prelude (
  Bool (..),
  Maybe (..),
  fst,
  head,
  map,
  take,
  takeByteString,
  ($),
  (.),
 )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude (IO, Semigroup ((<>)), mconcat)
import Prelude qualified as Haskell
import IntegriTree.Proofs (ProofTree(ProofLeaf))

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = do
  defaultMain $ do
    timeDeposit defaultBabbage

timeDeposit :: MockConfig -> TestTree
timeDeposit cfg = do
  testGroup
    -- The StateHolder only verifies that the output (only inline) datum has the following characteristics:
    --
    -- The vdAdatag field matches the minting policy's only token name.
    -- The operation is one greater than the input datum's operation value.
    -- The vdTreeSize is non-negative.
    -- The vdMintingPolicy is identical to the input datum's minting policy.
    --
    -- Additionally, the output and minting's value must only contain a CNFT token and no any other tokens
    --
    -- This simple rules will also satisfy the following constraints based on the Phase 1 validation pass:
    -- 1. There is only one input and output in the transaction.
    -- 2. Only one NFT is minted in the transaction.
    -- 3. The input's and output's token is the same.
    -- 4. The input is not a reference input.
    "Testing StateHolder"
    [ testGroup
        "Deploying tests"
        [good "Test Deploy StateHolder" testInitStateHolder]
    , testGroup
        -- These tests using valid datums,
        "Input, output and minting tests (with correct states)"
        [ bad "Must fail - Two inputs/outputs" $ testInputsOutputs 2 1 -- Only one input/ouput is allowed in the MVP's tx.
        , good "Must pass - One input and output" $ testInputsOutputs 1 1 -- 1 input, 1 output and 1 minting
        , bad "Must fail - One input and two outputs" $ testInputsOutputs 1 2 -- 1 input, 1 output and 1 minting
        , bad "Must fail - Valid input output but two mintings" $ testInputsOutputs 2 2 -- 1 input/output and 2 mintings
        ]
    , -- We assume a proper bootstrap, so the inputs must always be correct.
      -- Therefore, if there is any wrong input datum for a correct control NFT
      -- then the adatag is exploited, making it useless.
      -- Therefore we do not check input datums as they're assumed to be correct.
      testGroup
        "Output datum's sanity checks"
        [ bad "Must fail - wrong inline output datum (Unit)" $ testDatum "adatag" (dat1 "ab") datU -- Constr 1 [Constr 0 []]
        , bad "Must fail - malformed inline output datum ({ m = 1})" $ testDatum "adatag" (dat1 "adatag") datW -- Constr 3 [I 1]
        , bad "Must fail - malformed inline output datum (Empty)" $ testDatum "adatag" (dat1 "adatag") datE -- Constr 2 []
        , bad "Must fail - same correct inline input output datum" $ testDatum "adatag" (dat1 "aa") (dat1 "aa")
        , bad "Must fail - correct datum with wrong adatag" $ testDatum "adatag" datI (dat1 "adata")
        , good "Must pass - correct and valid inline input and output datum" $ testDatum "adatag" datI (dat1 "adatag")
        ]
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors {-Trace-} (adaValue 100_000_000) cfg

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------
goodTags :: [TokenName]
goodTags = ["adam", "brenda", "cecilia", "dennis"]

twoAda :: Value
twoAda = adaValue 2

datW :: ValidatorDatum
datW = WrongDatum {mock = 1}

datU :: ValidatorDatum
datU = UnitDatum ()

datE :: ValidatorDatum
datE = EmptyDatum {}

datI :: ValidatorDatum
datI = createDat 0 "" 0 mintSymbol

dat1 :: BuiltinByteString -> ValidatorDatum
dat1 a = createDat 1 a 1 mintSymbol

-- StateHolder only checks the op count, adatag, treesize and minting policy
createDat :: Integer -> BuiltinByteString -> Integer -> CurrencySymbol -> ValidatorDatum
createDat opc tag size mp =
  ValidatorDatum
    { vdOperationCount = opc
    , vdAdatag = tag
    , vdTreeState = AdatagAdded
    , vdTreeSize = size
    , vdTreeProof = ProofLeaf $ hash ""
    , vdMintingPolicy = mp
    }

---------------------------------------------------------------------------------------------------
------------------------------------- MOCK/REAL VALIDATORS ----------------------------------------

-- Always success minting policy for testing StateHolder
mockContract :: () -> ScriptContext -> Bool
mockContract _ _ = True

mockMintPolicy :: TypedPolicy ()
mockMintPolicy = mkTypedPolicy $$(PlutusTx.compile [||toBuiltinPolicy mockContract||])

mintSymbol :: CurrencySymbol
mintSymbol = scriptCurrencySymbol mockMintPolicy

-- StateHolder
type StateHolderScript = TypedValidator ValidatorDatum () -- No redeemer

stateHolderScript :: ControlNFT -> StateHolderScript
stateHolderScript c = mkTypedValidator $ stateHolderValidator c

-- Controlo NFT
cnftScript :: TxOutRef -> TypedPolicy ()
cnftScript ref = mkTypedPolicy $ CNFT.cnftPolicy ref CNFT.letters

---------------------------------------------------------------------------------------------------
---------------------------------------- UNIT TESTS -----------------------------------------------
-- Deploy StateHolder script
-- 1. Mint cnfts
-- 2. Send cnfts to the StateHolder
-- Returns cnft symbol, and ref address holding StateHolder and minting script
initValidator :: ValidatorDatum -> Run (StateHolderScript, CurrencySymbol)
initValidator idat = do
  -- minter
  u <- newUser $ adaValue 26 -- 1 ada for each cnft
  utxos <- utxoAt u

  let [(ref, out)] = utxos
      cnft = scriptCurrencySymbol (cnftScript ref)
      script = stateHolderScript cnft
      mintingValue = mconcat [singleton cnft tn 1 | tn <- CNFT.letters]

  -- mint cnft
  -- the received user has 26 ada and 26 cnfts
  submitTx u $ mintCnftTx ref out mintingValue u

  -- 3. Send cnfts fot the policy script
  v <- valueAt u
  sp <- spend u v

  -- send
  -- All init datum can be the same as no state check only sanity checks
  submitTx u $ deployTx sp script cnft idat

  return (script, cnft)
  where
    mintCnftTx ref out val pkh =
      mconcat
        [ mintValue (cnftScript ref) () val
        , payToKey pkh $ val <> txOutValue out
        , spendPubKey ref
        ]
    deployTx dsp scr curr dat =
      Haskell.mconcat
        [ userSpend dsp
        , Haskell.mconcat
            [ payToRef scr (InlineDatum dat) (adaValue 1 <> singleton curr tn 1) | tn <- CNFT.letters
            ]
        ]

-- Test deploying StateHolder
testInitStateHolder :: Run ()
testInitStateHolder = do
  -- Check that the StateHolder deployed correctly
  (vs, _) <- initValidator datI
  utxos <- utxoAt vs

  let (outref, _) = head utxos

  dat <- datumAt outref :: Run (Maybe ValidatorDatum)
  case dat of
    Just _ -> logInfo "Datum is good."
    _ -> logError "StateHolder is not deployed correctly: could not find datum"

-- Output datum related tests
testDatum :: BuiltinByteString -> ValidatorDatum -> ValidatorDatum -> Run ()
testDatum adatag idat odat = do
  u1 <- newUser $ adaValue 1_000

  (script, cnft) <- initValidator idat -- Initialize with the datum from parameter
  utxos <- utxoAt script
  sp <- spend u1 (adaValue 1)

  let (ref, _) = head utxos
      tx = datumTx script ref cnft u1 sp (TokenName adatag)

  submitTx u1 tx
  where
    datumTx scr ref cnft pkh sp at =
      mconcat
        [ mintValue mockMintPolicy () (singleton mintSymbol at 1)
        , spendScript scr ref () idat
        , userSpend sp
        , payToScript scr (InlineDatum odat) (adaValue 1 <> singleton cnft "a" 1)
        , payToKey pkh (adaValue 1 <> singleton mintSymbol at 1)
        ]

-- Sanity checks of inputs/ouptus of MVP. Only one input and putput is allowed
-- in the current version of @adatag.
testInputsOutputs :: Integer -> Integer -> Run ()
testInputsOutputs nri mnri = do
  u1 <- newUser $ adaValue 1_000

  (script, cnft) <- initValidator datI -- Initialize with the proper init datum
  utxos <- utxoAt script

  let refs = map fst $ take nri utxos
  let tns = take nri goodTags
  let mtns = take mnri goodTags
  --  let nms = if nri < mnri then mnri else nri

  sp <- spend u1 (adaValue 1)

  let mval = mconcat [singleton mintSymbol tn 1 | tn <- mtns]
      tx = inputTx script refs cnft u1 sp tns mval

  submitTx u1 tx
  where
    inputTx scr refs cnft pkh sp tns mv =
      mconcat
        [ mintValue mockMintPolicy () mv
        , -- \^ The minting's adatag can be the same with tokenname.
          mconcat [spendScript scr r () datI | r <- refs]
        , userSpend sp
        , mconcat [payToScript scr (InlineDatum (dat1 $ unTokenName tn)) (adaValue 1 <> singleton cnft (TokenName $ takeByteString 1 $ unTokenName tn) 1) | tn <- tns]
        , payToKey pkh (adaValue 1 <> mv)
        ]
