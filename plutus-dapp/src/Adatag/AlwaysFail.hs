{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Adatag.AlwaysFail where

import           PlutusTx         (BuiltinData, CompiledCode, compile)
import           PlutusTx.Prelude (error)
import           Prelude          (IO, String)
import           Utilities        (Network (..), validatorAddressBech32,
                                   writeCodeToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / StateHolder ------------------------------------------

-- It's a generic Always Fail StateHolder probably already exists on any of the networks.
{-# INLINEABLE mkAFValidator #-}
mkAFValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkAFValidator _ _ _ = error ()

-- Customised AFV.
-- traceError "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks"

alwaysFailValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
alwaysFailValidator = $$(PlutusTx.compile [||mkAFValidator||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveScript :: IO ()
saveScript = writeCodeToFile "contracts/01_afv.plutus" alwaysFailValidator

-- Generate the address to send the validators and minting scripts to referencing them from.
-- Network details, from Shelley genesis file, at
-- https://book.world.dev.cardano.org/environments.html
-- Network IDs and magics:sa
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

-- ghci> fromNetworkMagic (NetworkMagic 2)
-- Testnet (NetworkMagic {unNetworkMagic = 2})

referenceAddressBech32 :: Network -> String
referenceAddressBech32 network = validatorAddressBech32 network alwaysFailValidator
