{-# LANGUAGE LambdaCase #-}

module Utilities.Serialise
  ( validatorToScript,
    policyToScript,
    codeToScript,
    writeCodeToFile,
    --  , writeValidatorToFile
    --   , writePolicyToFile
    dataToJSON,
    printDataToJSON,
    writeDataToFile,
  )
where

import Cardano.Api (Error (displayError), File (..), PlutusScript, PlutusScriptV2, prettyPrintJSON, unsafeHashableScriptData, writeFileJSON, writeFileTextEnvelope)
import Cardano.Api.Shelley
  ( PlutusScript (..),
    fromPlutusData,
    scriptDataToJsonDetailedSchema,
  )
import Codec.Serialise (Serialise, serialise)
import Data.Aeson (Value)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import PlutusLedgerApi.V1 (ToData)
import qualified PlutusLedgerApi.V2 as PlutusV2
import PlutusTx (CompiledCode)
import Text.Printf (printf)

serializableToScript :: (Serialise a) => a -> PlutusScript PlutusScriptV2
serializableToScript = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

-- Serialize compiled code
codeToScript :: CompiledCode a -> PlutusScript PlutusScriptV2
codeToScript = serializableToScript . PlutusV2.serialiseCompiledCode

-- Serialize validator
validatorToScript :: CompiledCode a -> PlutusScript PlutusScriptV2
validatorToScript = serializableToScript . PlutusV2.serialiseCompiledCode

-- Serialize minting policy
policyToScript :: CompiledCode a -> PlutusScript PlutusScriptV2
policyToScript = serializableToScript . PlutusV2.serialiseCompiledCode

-- Create file with Plutus script
writeScriptToFile :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
writeScriptToFile filePath script =
  writeFileTextEnvelope (File filePath) Nothing script >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "Serialized script to: " ++ filePath

-- Create file with compiled code
writeCodeToFile :: FilePath -> CompiledCode a -> IO ()
writeCodeToFile filePath = writeScriptToFile filePath . codeToScript

-- Create file with compiled Plutus validator
-- FIXME: writeValidatorToFile :: FilePath -> CompiledCode  -> IO ()
-- writeValidatorToFile filePath = writeScriptToFile filePath . validatorToScript

-- Create file with compiled Plutus minting policy
-- FIXME: writePolicyToFile :: FilePath -> Serialise -> IO ()
-- writePolicyToFile filePath = writeScriptToFile filePath . policyToScript

dataToJSON :: (ToData a) => a -> Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . PlutusV2.toData

printDataToJSON :: (ToData a) => a -> IO ()
printDataToJSON = putStrLn . BS8.unpack . prettyPrintJSON . dataToJSON

writeDataToFile :: (ToData a) => FilePath -> a -> IO ()
writeDataToFile filePath x = do
  let v = dataToJSON x
  writeFileJSON filePath v >>= \case
    Left err -> print $ displayError err
    Right () -> printf "Wrote data to: %s\n%s\n" filePath $ BS8.unpack $ prettyPrintJSON v
