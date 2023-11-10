{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Contracts.AlwaysFail where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx (BuiltinData, compile)
import PlutusTx.Prelude (error)
import Utilities (Network (..), validatorAddressBech32, writeValidatorToFile)
import Prelude (IO, String)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- It's a generic Always Fail Validator probably already exists on any of the networks.
{-# INLINEABLE mkAFValidator #-}
mkAFValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkAFValidator _ _ _ = error ()

-- Customised AFV.
-- traceError "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks"

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [||mkAFValidator||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/01_afv.plutus" validator

-- Generate the address to send the validators and minting script.
-- Network details, from Shelley genesis file, at
-- https://book.world.dev.cardano.org/environments.html
-- Network IDs and magics:
-- Sancho   = (Testnet,4)
-- Preview  = (Testnet,2)
-- Pre-Prod = (Testnet,1)
-- Mainnet  = (Mainnet,764824073)
-- The address generation only uses the Netork IDs e.g.: Mainnet=0 and Testnet=1
-- Example (these simplest AFV addresses already exists on all of the networks):

-- $ cabal repl
-- Prelude AlwaysFailValidator> import Utilities
-- Prelude Utilities AlwaysFailValidator> referenceAddressBech32 Testnet
-- "addr_test1wpgexmeunzsykesf42d4eqet5yvzeap6trjnflxqtkcf66g0kpnxt"
-- Prelude Utilities AlwaysFailValidator> referenceAddressBech32 Mainnet
-- "addr1w9gexmeunzsykesf42d4eqet5yvzeap6trjnflxqtkcf66g5740fw"

referenceAddressBech32 :: Network -> String
referenceAddressBech32 network = validatorAddressBech32 network validator