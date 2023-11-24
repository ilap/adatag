{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import qualified Adatag.AdatagMinting       as ATM
import qualified Adatag.AlwaysFail          as AFV
import qualified Adatag.ControlNFTMinting   as CNM
import qualified Adatag.StateHolder         as ATV
import qualified Adatag.TimeDeposit         as TDV
import           Cardano.Api.Shelley        hiding (ScriptHash, TxId)
import qualified Data.ByteString.Char8      as BS8
import           Data.Text
import           Data.Time.Clock
import           Data.Time.Format
import           GHC.Exts                   (fromList)
import           PlutusLedgerApi.V2         (PubKeyHash)
import           PlutusLedgerApi.V2.Tx
import           PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import           PlutusTx.Prelude           (fromMaybe)
import           System.IO                  ()
import           Utilities                  (Network (..), bytesFromHex,
                                             posixTimeFromIso8601, scriptHash,
                                             scriptHash')


-- It does not require handling utf-8, as these are ASCII letters.
hexToBS :: String -> BuiltinByteString
hexToBS s = BuiltinByteString $ bytesFromHex (BS8.pack s)


bech32ToPubkeyHash :: Text -> Maybe PubKeyHash
bech32ToPubkeyHash address = do
  let addr = case deserialiseFromBech32 AsShelleyAddress address of -- (BS8.pack address) of
        Right a            -> a
        Left decodingError -> error (show decodingError)
  case shelleyPayAddrToPlutusPubKHash addr of
    pkh -> pkh
    _   -> Nothing

data Networks
  = Sancho
  | Preview
  | Preprod
  | Mainnet
  deriving (Prelude.Show)

usedSettings :: Networks -> (Network, String)
usedSettings nws = case nws of
  Main.Sancho  -> (Utilities.Testnet, "https://sancho.cardanoscan.io")
  Main.Preview -> (Utilities.Testnet, "https://preview.cardanoscan.io")
  Main.Preprod -> (Utilities.Testnet, "https://preprod.cardanoscan.io")
  Main.Mainnet -> (Utilities.Mainnet, "https://cardanoscan.io")

toBS :: String -> BuiltinByteString
toBS s = BuiltinByteString $ bytesFromHex (BS8.pack s)

createOref :: String -> Integer -> TxOutRef
createOref tx = TxOutRef (TxId $ toBS tx)

{-
This file compiles all the validators and minting scripts required for @adatag
-}
main :: IO ()
main = do
  -- User inputs
  -- Network
  let env = Preview
  let (network, url) = usedSettings env -- Preprod, Mainnet

  -- Check you're using the right UTxO from the correct network above
  let txId = "97a2841803f778bd3609d053b83c9837420c34359b5a6ba979a0dd01af5ff382"
  let idx = 1 :: Integer

  let txIdUrl = url ++ "/transaction/" ++ txId

  --  Address to collect the unclaimed time deposit locks.
  let collectionAddress = "addr1q8dw5cld2qz3t4lqa2sycl0z7z782zmgce6na08mmnje7rjcnqa3jeyd7z5h6670654h2nq4d7pmc7kseruqydq3w3squafaml"

  let deactivationDaysAfter = 183 --  approx. half year after bootstrap
  let collectionDaysAfter = 365 -- approx a year after bootsrap
  let lockPeriod = 20 -- in days
  putStrLn "############ Bootstrap details ##############"
  putStrLn ""

  putStrLn "------------------ Common -------------------"
  putStrLn $ "Network (" ++ show env ++ ") : " ++ show network -- Required for generating addresses
  putStrLn $ "Collector address : " ++ url ++ "/address/" ++ collectionAddress
  putStrLn "---------------------------------------------"
  putStrLn ""

  let refAddress = AFV.referenceAddressBech32 network
  let afvPolicyId = scriptHash' AFV.alwaysFailValidator
  putStrLn "----------- 1. Always Fail Validator -----------"
  putStrLn $ "Reference address : " ++ refAddress -- Address the Validator and Minting scripts are sent as reference scripts.
  putStrLn $ "Url               : " ++ url ++ "/address/" ++ refAddress
  putStrLn $ "Own policy ID     : " ++ show afvPolicyId
  putStrLn "------------------- Saving --------------------"
  -- Save script
  AFV.saveScript
  putStrLn "-----------------------------------------------"
  putStrLn ""

  let oref = createOref txId idx
  let cnftSymbol = CNM.controlNFTCurrencySymbol oref
  let cnftPolicyID = scriptHash $ CNM.cnftPolicy oref CNM.letters
  putStrLn "----- 2. Control NFT (CNFT) Minting Script -----"
  putStrLn $ "One-shot UTxO     : " ++ txId ++ "#" ++ show idx
  putStrLn $ "Own-hot UTxO Url  : " ++ show txIdUrl
  putStrLn $ "Own policy ID     : " ++ show cnftPolicyID
  putStrLn "------------------- Saving --------------------"

  -- Save script
  CNM.saveControlNFTMintingPolicy oref
  putStrLn "-----------------------------------------------"
  putStrLn ""

  now <- getCurrentTime
  let deactivationTime = formatTime defaultTimeLocale "%FT%TZ" $ addUTCTime (60 * 60 * 24 * deactivationDaysAfter) now -- half year
  let collectionTime = formatTime defaultTimeLocale "%FT%TZ" $ addUTCTime (60 * 60 * 24 * collectionDaysAfter) now -- a year
  let collectionPkh = bech32ToPubkeyHash $ fromList collectionAddress
  let cp = fromMaybe (error "Invalid PubKeyHash.") collectionPkh
  let ct = fromMaybe (error "Invalid Time string.") $ posixTimeFromIso8601 collectionTime
  let tdp = TDV.TimeDepositParams {TDV.dpCollector = cp, TDV.dpCollectionTime = ct}

  let timdedepositValidator = scriptHash' $ TDV.timeDepositValidator tdp
  let receiveAddress = TDV.timeDepositAddressBech32 network tdp
  putStrLn "----------- 3. Time Deposit Validator ----------"
  putStrLn $ "Own addr (deposit to send) : " ++ receiveAddress
  putStrLn $ "Own policy ID              : " ++ show timdedepositValidator
  putStrLn "----------------  Parameters  -----------------"
  putStrLn $ "1. Collector Pkh   : " ++ show collectionPkh
  putStrLn $ "2. Collection time : " ++ collectionTime
  putStrLn "------------------- Saving --------------------"
  -- Save script
  TDV.saveTimeDepositValidator cp ct
  putStrLn "-----------------------------------------------"
  putStrLn ""

  let stateHolderValidator = ATV.valHashBySymbol cnftSymbol

  putStrLn "------- 4. StateHolder Validator --------"
  putStrLn $ "Own policy ID             : " ++ show stateHolderValidator
  putStrLn $ "Reference address to send : " ++ url ++ "/address/" ++ refAddress
  putStrLn "----------------  Parameters  -----------------"
  putStrLn $ "1. CNFT Currency Symbol : " ++ show cnftSymbol
  putStrLn "!!!IMPORTANT!!!"
  putStrLn "The minting policy id will be included in the"
  putStrLn "StateHolder's datum at initialisation time, as"
  putStrLn "both must cross eference each other."
  putStrLn "------------------- Saving --------------------"
  -- Save script
  ATV.saveStateHolderValidator cnftSymbol
  putStrLn "-----------------------------------------------"
  putStrLn ""

  let minDeposit = 1750 :: Integer
  let dt = fromMaybe (error "Invalid Time string.") $ posixTimeFromIso8601 deactivationTime

  putStrLn "---------- 5. Adatag Minting Script ----------"

  putStrLn $ "Deactivation time : " ++ deactivationTime

  putStrLn "----------------  Parameters  -----------------"
  putStrLn $ "1. Control NFT Symbol       : " ++ show cnftSymbol
  putStrLn $ "2. StateHolder policy Id      : " ++ show stateHolderValidator
  putStrLn $ "3. Time Deposit policy Id   : " ++ show timdedepositValidator
  putStrLn $ "4. Time Deposit Lock expiry : " ++ show deactivationTime
  putStrLn $ "5. Time Deposit Lock period : " ++ show lockPeriod
  putStrLn $ "6. Time Deposit max deposit : " ++ show minDeposit
  putStrLn "------------------- Saving --------------------"
  -- Save script
  ATM.saveAdatagMintingPolicy cnftSymbol stateHolderValidator timdedepositValidator dt lockPeriod minDeposit
  putStrLn "-----------------------------------------------"
