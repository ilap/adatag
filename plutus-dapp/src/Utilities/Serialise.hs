{-# LANGUAGE LambdaCase #-}

module Utilities.Serialise
  ( codeToScript,
    dataToJSON,
    printDataToJSON,
    writeCodeToFile,
    writeDataToFile,
  )
where

import           Cardano.Api           (Error (displayError), File (..),
                                        PlutusScript, PlutusScriptV2,
                                        prettyPrintJSON,
                                        unsafeHashableScriptData, writeFileJSON,
                                        writeFileTextEnvelope)
import           Cardano.Api.Shelley   (PlutusScript (..), fromPlutusData,
                                        scriptDataToJsonDetailedSchema)
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as BS8
import           PlutusLedgerApi.V1    (ToData)
import qualified PlutusLedgerApi.V2    as PlutusV2
import           PlutusTx              (CompiledCode)
import           Text.Printf           (printf)

-- Serialize compiled code
codeToScript :: CompiledCode a -> PlutusScript PlutusScriptV2
codeToScript = PlutusScriptSerialised . PlutusV2.serialiseCompiledCode

-- Create file with Plutus script
writeScriptToFile :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
writeScriptToFile filePath script =
  writeFileTextEnvelope (File filePath) Nothing script >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "Serialized script to: " ++ filePath

-- Create file with compiled code
writeCodeToFile :: FilePath -> CompiledCode a -> IO ()
writeCodeToFile filePath = writeScriptToFile filePath . codeToScript

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
