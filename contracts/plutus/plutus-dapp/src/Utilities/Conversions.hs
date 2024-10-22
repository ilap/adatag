{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Utilities.Conversions (
  Network (..),
  currencySymbol,
  hashScript,
  posixTimeFromIso8601,
  posixTimeToIso8601,
  bytesFromHex,
  bytesToHex,
  validatorAddressBech32,
  scriptHash,
  scriptHash',
  tryReadAddress,
)
where

import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as Api
import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.BaseTypes (
  CertIx (CertIx),
  Network (..),
  TxIx (TxIx),
 )
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Crypto qualified as Ledger
import Cardano.Ledger.Hashes qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as BS16
import Data.Text qualified as Text
import Data.Time.Clock.POSIX qualified as Time
import Data.Time.Format.ISO8601 qualified as Time
import PlutusLedgerApi.V2 (
  CurrencySymbol (CurrencySymbol),
  POSIXTime,
  ScriptHash (..),
 )
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusTx (CompiledCode)
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Utilities.Serialise (codeToScript)

hashScript :: Api.PlutusScript Api.PlutusScriptV2 -> Api.ScriptHash
hashScript = Api.hashScript . Api.PlutusScript Api.PlutusScriptV2

scriptHash :: CompiledCode a -> Api.ScriptHash
scriptHash = hashScript . codeToScript

scriptHash' :: CompiledCode a -> ScriptHash
scriptHash' = ScriptHash . BuiltinByteString . Api.serialiseToRawBytes . hashScript . codeToScript

currencySymbol :: CompiledCode a -> CurrencySymbol
currencySymbol = CurrencySymbol . BuiltinByteString . Api.serialiseToRawBytes . hashScript . codeToScript

validatorAddressBech32 :: Network -> CompiledCode a -> String
validatorAddressBech32 network v =
  Text.unpack
    $ Api.serialiseToBech32
    $ Api.ShelleyAddress
      network
      (Ledger.ScriptHashObj $ Api.toShelleyScriptHash $ scriptHash v)
      Ledger.StakeRefNull

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

{-- Builtintype of bytesFromHex.
import qualified PlutusLedgerApi.V1.Bytes as P (bytes, fromHex)
bytesFromHex :: ByteString -> BuiltinByteString
bytesFromHex = toBuiltin . P.bytes . fromEither . P.fromHex
  where
    fromEither (Left _)   = traceError "bytesFromHex failed"
    fromEither (Right bs) = bs
-}

credentialLedgerToPlutus :: Ledger.Credential a Ledger.StandardCrypto -> Plutus.Credential
credentialLedgerToPlutus (Ledger.ScriptHashObj (Ledger.ScriptHash h)) = Plutus.ScriptCredential $ Plutus.ScriptHash $ Plutus.toBuiltin $ hashToBytes h
credentialLedgerToPlutus (Ledger.KeyHashObj (Ledger.KeyHash h)) = Plutus.PubKeyCredential $ Plutus.PubKeyHash $ Plutus.toBuiltin $ hashToBytes h

stakeReferenceLedgerToPlutus :: Ledger.StakeReference Ledger.StandardCrypto -> Maybe Plutus.StakingCredential
stakeReferenceLedgerToPlutus (Ledger.StakeRefBase x) =
  Just $ Plutus.StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (Ledger.StakeRefPtr (Ledger.Ptr (Api.SlotNo x) (TxIx y) (CertIx z))) =
  Just $ Plutus.StakingPtr (fromIntegral x) (fromIntegral y) (fromIntegral z)
stakeReferenceLedgerToPlutus Ledger.StakeRefNull =
  Nothing

tryReadAddress :: String -> Maybe Plutus.Address
tryReadAddress x = case Api.deserialiseAddress Api.AsAddressAny $ Text.pack x of
  Nothing -> Nothing
  Just (Api.AddressByron _) -> Nothing
  Just (Api.AddressShelley (Api.ShelleyAddress _ p s)) ->
    Just
      Plutus.Address
        { Plutus.addressCredential = credentialLedgerToPlutus p
        , Plutus.addressStakingCredential = stakeReferenceLedgerToPlutus s
        }
