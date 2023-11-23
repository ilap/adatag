{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Utilities.Conversions
  ( Network (..),
    currencySymbol,
    posixTimeFromIso8601,
    posixTimeToIso8601,
    bytesFromHex,
    bytesToHex,
  )
where

import qualified Cardano.Api as Api
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (Network (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
--  MintingPolicy,
--  MintingPolicyHash (MintingPolicyHash),
{-, Validator-}

import qualified Data.ByteString.Short as SBS
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.Format.ISO8601 as Time
import PlutusLedgerApi.V2
  ( CurrencySymbol (CurrencySymbol),
    POSIXTime,
    ScriptHash (..),
    serialiseCompiledCode,
  )
import PlutusTx (CompiledCode)
import PlutusTx.Builtins (toBuiltin)
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Utilities.Serialise (policyToScript)

hashScript :: Api.PlutusScript Api.PlutusScriptV2 -> Api.ScriptHash
hashScript = Api.hashScript . Api.PlutusScript Api.PlutusScriptV2

-- FIXME: validatorHash :: Validator -> Api.ScriptHash
-- validatorHash = hashScript . validatorToScript

-- validatorHash' :: Validator -> Plutus.ValidatorHash
-- validatorHash' = Plutus.ValidatorHash . BuiltinByteString . Api.serialiseToRawBytes . hashScript . validatorToScript

-- FIXME: policyHash :: MintingPolicy -> MintingPolicyHash
-- policyHash = MintingPolicyHash . BuiltinByteString . Api.serialiseToRawBytes . hashScript . policyToScript

-- currencySymbol :: Api.PlutusScript Api.PlutusScriptV2 -> CurrencySymbol
-- currencySymbol = CurrencySymbol . BuiltinByteString . Api.serialiseToRawBytes .  hashScript . policyToScript

currencySymbol :: CompiledCode a -> CurrencySymbol
currencySymbol = CurrencySymbol . BuiltinByteString . Api.serialiseToRawBytes . hashScript . policyToScript

-- currencySymbol2 :: CompiledCode a  -> CurrencySymbol
-- currencySymbol2 = CurrencySymbol  . Plutus.serialiseCompiledCode hashScript2

hashScript2 :: CompiledCode a -> PlutusLedgerApi.V2.ScriptHash
hashScript2 =
  PlutusLedgerApi.V2.ScriptHash
    . toBuiltin
    . (Hash.hashToBytes :: Hash.Hash Hash.Blake2b_224 SBS.ShortByteString -> BS.ByteString)
    . Hash.hashWith (BS.append "\x02" . SBS.fromShort) -- For Plutus V2.
    . serialiseCompiledCode

{-
scriptCurrencySymbol :: Api.PlutusScript Api.PlutusScriptV2 -> CurrencySymbol
scriptCurrencySymbol policy =
  C.transPolicyID $
    C.PolicyID $
      C.hashScript @(AlonzoEra StandardCrypto) . Api.PlutusScript Api.PlutusScriptV2
-}

{-validatorAddressBech32 :: Network -> Api.PlutusScript Api.PlutusScriptV2 -> String
validatorAddressBech32 network v =
     Text.unpack $
     Api.serialiseToBech32 $
    Api.ShelleyAddress
        network
        (ScriptHashObj $ Api.toShelleyScriptHash $ hashScript v) --  $ validatorHash v)
        StakeRefNull
-}
posixTimeFromIso8601 :: String -> Maybe POSIXTime
posixTimeFromIso8601 s = do
  t <- Time.formatParseM Time.iso8601Format s
  let seconds = Time.utcTimeToPOSIXSeconds t
      milliSeconds = round $ 1000 * seconds :: Integer
  return $ fromInteger milliSeconds

posixTimeToIso8601 :: POSIXTime -> String
posixTimeToIso8601 t =
  Time.formatShow Time.iso8601Format $ Time.posixSecondsToUTCTime $ fromRational $ toRational t / 1000

bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = either error id . BS16.decode

bytesToHex :: BS.ByteString -> BS.ByteString
bytesToHex = BS16.encode
