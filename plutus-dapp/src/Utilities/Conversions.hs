{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds         #-}
--{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Utilities.Conversions
  ( Network (..)
 --  , validatorHash
 -- , validatorHash'
--  , policyHash
  ,  currencySymbol
  --, validatorAddressBech32
  , posixTimeFromIso8601
  , posixTimeToIso8601
  , bytesFromHex
  , bytesToHex
  --, tryReadAddress
  ) where

import qualified Cardano.Api                 as Api
import           Cardano.Api.Shelley         (Address (..))
import qualified Cardano.Api.Shelley         as Api
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import qualified Cardano.Crypto.Hash  as Hash
import           Cardano.Ledger.BaseTypes    (CertIx (..), Network (..),
                                              TxIx (..))
import           Cardano.Ledger.Credential   as Ledger
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Cardano.Ledger.Hashes       (ScriptHash (..))
import           Cardano.Ledger.Keys         (KeyHash (..))
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base16      as BS16
import           Data.Text                   (pack)
import qualified Data.Text                   as Text
import qualified Data.Time.Clock.POSIX       as Time
import qualified Data.Time.Format.ISO8601    as Time
import           PlutusLedgerApi.V1.Credential as Plutus
import           PlutusLedgerApi.V1.Crypto     as Plutus
import           PlutusLedgerApi.V2        (CurrencySymbol (CurrencySymbol),
                                             --  MintingPolicy,
                                             --  MintingPolicyHash (MintingPolicyHash),
                                              POSIXTime {-, Validator-}, ScriptHash (..), serialiseCompiledCode)
import qualified PlutusLedgerApi.V2        as Plutus
import           PlutusTx.Builtins           (toBuiltin)
import           PlutusTx.Builtins.Internal  (BuiltinByteString (..))
import PlutusLedgerApi.V1.Address (toScriptHash)
import PlutusTx (CompiledCode)
import Utilities.Serialise (policyToScript)
-- FIXME:  -- import           Utilities.Serialise         (policyToScript, validatorToScript)
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Short  as SBS

{-
import qualified Cardano.Crypto.Hash.Class  as C
import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo  as C
import qualified Cardano.Ledger.Alonzo.Data  as C
import qualified Cardano.Ledger.Alonzo.Language  as C
import qualified Cardano.Ledger.Alonzo.Scripts  as C
import qualified Cardano.Ledger.Alonzo.TxInfo  as C
import qualified Cardano.Ledger.Babbage.TxInfo  as C (transScriptHash)
import qualified Cardano.Ledger.Core  as C (hashScript)
import qualified Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Mary.Value  as C
import qualified Cardano.Ledger.SafeHash  as C
import qualified Cardano.Simple.PlutusLedgerApi.V1.Scripts  as P
import qualified PlutusLedgerApi.V2  as P
import Utilities.Serialise (policyToScript)
-}

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

currencySymbol :: CompiledCode a  -> CurrencySymbol
currencySymbol = CurrencySymbol . BuiltinByteString . Api.serialiseToRawBytes . hashScript . policyToScript


--currencySymbol2 :: CompiledCode a  -> CurrencySymbol
--currencySymbol2 = CurrencySymbol  . Plutus.serialiseCompiledCode hashScript2

hashScript2 :: CompiledCode a -> PlutusLedgerApi.V2.ScriptHash
hashScript2 =
  PlutusLedgerApi.V2.ScriptHash
    . toBuiltin
    . (Hash.hashToBytes :: Hash.Hash Hash.Blake2b_224 SBS.ShortByteString -> BS.ByteString)
    . Hash.hashWith (BS.append "\x02" . SBS.fromShort)  -- For Plutus V2.
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
