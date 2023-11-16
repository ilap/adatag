{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Contracts.ControlNFTMinting as CNFT
import Contracts.Validator
import Control.Monad (Monad (return))
import Plutus.Model.V2
import Plutus.V1.Ledger.Value (CurrencySymbol (..), TokenName (unTokenName))
import Plutus.V2.Ledger.Api
  ( BuiltinByteString,
    ScriptContext,
    TokenName (TokenName),
    TxOut (txOutValue),
    TxOutRef,
    Value,
    singleton,
  )
import qualified PlutusTx
import PlutusTx.Builtins (Integer)
import PlutusTx.Prelude (Bool (..), Maybe (..), fst, head, map, take, ($), (*), (.), (<))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude (IO, Semigroup ((<>)), mconcat)
import qualified Prelude as Haskell

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = do
  defaultMain $ do
    timeDeposit defaultBabbage

timeDeposit :: MockConfig -> TestTree
timeDeposit cfg = do
  testGroup
    -- The validator only verifies that the output (only inline) datum has the following characteristics:
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
    "Testing state holder (Control NFT) Validator"
    [ testGroup
        "Deploying tests"
        [good "Test Deploy validator" testInitValidator],
      testGroup
        -- These tests using valid datums,
        "Input, output and minting tests (with correct states)"
        [ bad "Invalid - Two inputs/outputs" $ testInputsOutputs 2 1, -- Only one input/ouput is allowed in the MVP's tx.
          good "Valid   - One input and output" $ testInputsOutputs 1 1, -- 1 input, 1 output and 1 minting
          bad "Invalid - Valid input output but two mintings" $ testInputsOutputs 1 2 -- 1 input/output and 2 mintings
        ],
      -- We assume a proper bootstrap, so the inputs must always be correct.
      -- Therefore, if there is any wrong input datum for a correct control NFT
      -- then the adatag is exploited, making it useless.
      -- Therefore we do not check input datums as they're assumed to be correct.
      testGroup
        "Output datum's sanity checks"
        [ bad "Invalid - wrong inline output datum (Unit)" $ testDatum "adatag" (dat1 "ab") datU, -- Constr 1 [Constr 0 []]
          bad "Invalid - malformed inline output datum ({ m = 1})" $ testDatum "adatag" (dat1 "adatag") datW, -- Constr 3 [I 1]
          bad "Invalid - malformed inline output datum (Empty)" $ testDatum "adatag" (dat1 "adatag") datE, -- Constr 2 []
          bad "Invalid - same correct inline input output datum" $ testDatum "adatag" (dat1 "aa") (dat1 "aa"),
          bad "Invalid - correct datum with wrong adatag" $ testDatum "adatag" datI (dat1 "adata"),
          good "Valid   - correct and valid inline input and output datum" $ testDatum "adatag" datI (dat1 "adatag")
        ]
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors{-Trace-} (adaValue 100_000_000) cfg

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

-- Validator only checks the op count, adatag, treesize and minting policy
createDat :: Integer -> BuiltinByteString -> Integer -> CurrencySymbol -> ValidatorDatum
createDat opc tag size mp =
  ValidatorDatum
    { vdOperationCount = opc,
      vdAdatag = tag,
      vdTreeState = AdatagAdded,
      vdTreeSize = size,
      vdTreeProof = "000000000000",
      vdMintingPolicy = mp
    }

---------------------------------------------------------------------------------------------------
------------------------------------- MOCK/REAL VALIDATORS ----------------------------------------

-- Always success minting policy for testing validator
{-# INLINEABLE mockContract #-}
mockContract :: () -> ScriptContext -> Bool
mockContract _ _ = True

mockMintPolicy :: TypedPolicy ()
mockMintPolicy = mkTypedPolicy $$(PlutusTx.compile [||toBuiltinPolicy mockContract||])

mintSymbol :: CurrencySymbol
mintSymbol = scriptCurrencySymbol mockMintPolicy

-- Validator
type ValidatorScript = TypedValidator ValidatorDatum () -- No redeemer

validatorScript :: ControlNFT -> ValidatorScript
validatorScript c = TypedValidator $ toV2 $ stateHolderValidator c

-- Controlo NFT

cnftScript :: TxOutRef -> TypedPolicy ()
cnftScript ref = TypedPolicy . toV2 $ CNFT.cnftPolicy ref CNFT.letters

---------------------------------------------------------------------------------------------------
---------------------------------------- UNIT TESTS -----------------------------------------------

-- Deploy Validator script
-- 1. Mint cnfts
-- 2. Send cnfts to the validator
-- 3. Load refscript to the refuser (Always Fail validator, but now it's a simple user)
-- Returns cnft symbol, and ref address holding validator and minting script
initValidator :: ValidatorDatum -> Run (ValidatorScript, CurrencySymbol)
initValidator idat = do
  -- minter
  u <- newUser $ adaValue 26 -- 1 ada for each cnft
  utxos <- utxoAt u

  let [(ref, out)] = utxos
      cnft = CNFT.controlNFTCurrencySymbol ref
      script = validatorScript cnft
      mintingValue = mconcat [singleton cnft tn 1 | tn <- CNFT.letters]

  -- mint cnft
  -- the received user has 26 ada and 26 cnfts
  submitTx u $ mintCnftTx ref out mintingValue u

  -- 2. Load validators reference address
  -- The current PSM loads only to the same script address instead of any individual address.
  -- ref user (not really true)
  u1 <- newUser twoAda
  sp1 <- spend u1 twoAda

  submitTx u1 $ loadScriptsTx sp1 script

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
        [ mintValue (cnftScript ref) () val,
          payToKey pkh $ val <> txOutValue out,
          spendPubKey ref
        ]
    loadScriptsTx lsp scr =
      mconcat
        [ userSpend lsp,
          loadRefScript scr twoAda
        ]
    deployTx dsp scr curr dat =
      Haskell.mconcat
        [ userSpend dsp,
          Haskell.mconcat
            [ payToRef scr (InlineDatum dat) (adaValue 1 <> singleton curr tn 1) | tn <- CNFT.letters
            ]
        ]

-- Test deploying validator
testInitValidator :: Run ()
testInitValidator = do
  -- Check that the validator deployed correctly
  (vs, _) <- initValidator datI
  utxos <- utxoAt vs

  let (outref, _) = head utxos

  dat <- datumAt outref :: Run (Maybe ValidatorDatum)
  case dat of
    Just _ -> logInfo "Datum is good."
    _ -> logError "Validator is not deployed correctly: could not find datum"

-- Output datum related tests
testDatum :: BuiltinByteString -> ValidatorDatum -> ValidatorDatum -> Run ()
testDatum adatag idat odat = do
  u1 <- newUser $ adaValue 1_000

  (script, cnft) <- initValidator idat -- Initialize with the datum from parameter
  utxos <- utxoAt script
  sp <- spend u1 (adaValue 1)

  let (ref, _) = head utxos

  let tx = datumTx script ref cnft u1 sp (TokenName adatag)
  logInfo $ "Transaction details: \n" <> Haskell.show tx
  submitTx u1 tx
  where
    datumTx scr ref cnft pkh sp at =
      mconcat
        [ mintValue mockMintPolicy () (singleton mintSymbol at 1),
          spendScript scr ref () idat,
          userSpend sp,
          payToScript scr (InlineDatum odat) (adaValue 1 <> singleton cnft "a" 1),
          payToKey pkh (adaValue 1 <> singleton mintSymbol at 1)
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
  let nms = if nri < mnri then mnri else nri

  sp <- spend u1 (adaValue $ 1 * nms)

  let tx = inputTx script refs cnft u1 sp (take nri CNFT.letters) tns mtns
  logInfo $ "Transaction details: \n" <> Haskell.show refs
  submitTx u1 tx
  where
    inputTx scr refs cnft pkh sp ls tns mtns =
      mconcat
        [ mconcat [mintValue mockMintPolicy () (singleton mintSymbol tn 1) | tn <- mtns], -- The minting's adatag can be the same with tokenname.
          mconcat [spendScript scr r () datI | r <- refs],
          userSpend sp,
          mconcat [payToScript scr (InlineDatum (dat1 $ unTokenName tn)) (adaValue 1 <> singleton cnft l 1) | l <- ls, tn <- tns],
          mconcat [payToKey pkh (adaValue 1 <> singleton mintSymbol tn 1) | tn <- mtns]
        ]
