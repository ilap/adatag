{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Cardano.Api
import Cardano.Api.Shelley
import qualified Contracts.AdatagMinting as ATM
import qualified Contracts.AlwaysFail as AFV
import qualified Contracts.ControlNFTMinting as CNM
import qualified Contracts.TimeDeposit as TDV
import qualified Contracts.Validator as ATV
import qualified Data.Text as Text
import Data.Time.Clock
import Data.Time.Format
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), TokenName (TokenName, unTokenName), Value, adaSymbol, flattenValue, singleton, unAssetClass)
import Plutus.V2.Ledger.Api (CurrencySymbol (CurrencySymbol), POSIXTime (POSIXTime), PubKeyHash)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import PlutusTx.Prelude (fromMaybe, takeByteString, traceError)
import System.IO ()
import Utilities (Network (..), policyHash, posixTimeFromIso8601, posixTimeToIso8601, validatorHash')

bech32ToPubkeyHash :: String -> Maybe PubKeyHash
bech32ToPubkeyHash address = do
  let addr = case deserialiseFromBech32 AsShelleyAddress (Text.pack address) of
        Right a -> a
        Left decodingError -> error (show decodingError)
  case shelleyPayAddrToPlutusPubKHash addr of
    pkh -> pkh
    _ -> Nothing

data Networks
  = Preview
  | Preprod
  | Mainnet
  deriving (Prelude.Show)

-- Adahandle is not available in Preprod and Preview.
-- For testing Purpose a pubkey based NFT minting/burning policy will be created
-- for creating mock ada handles.
usedSettings :: Networks -> (Network, String, String)
usedSettings nws = case nws of
  Main.Preview -> (Utilities.Testnet, "8d18d786e92776c824607fd8e193ec535c79dc61ea2405ddf3b09fe3", "https://preview.cardanoscan.io")
  Main.Preprod -> (Utilities.Testnet, "8d18d786e92776c824607fd8e193ec535c79dc61ea2405ddf3b09fe3", "https://preprod.cardanoscan.io")
  Main.Mainnet -> (Utilities.Mainnet, "f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a", "https://cardanoscan.io")

{-
This file compiles all the validators and minting scripts required for @adatag
-}
main :: IO ()
main = do
  -- User inputs
  -- Network
  let env = Preview
  let (network, adahandle, url) = usedSettings env -- Preprod, Mainnet

  -- Check you're using the right UTxO from the correct network above
  let txId = "97a2841803f778bd3609d053b83c9837420c34359b5a6ba979a0dd01af5ff382"
  let idx = 1 :: Integer

  let txIdUrl = url ++ "/transaction/" ++ txId

  --  Address to collect the unclaimed time deposit locks.
  let collectionAddress = "addr1q8dw5cld2qz3t4lqa2sycl0z7z782zmgce6na08mmnje7rjcnqa3jeyd7z5h6670654h2nq4d7pmc7kseruqydq3w3squafaml"

  let deactivationDaysAfter = 183 --  approx. half year after bootstrap
  let collectionDaysAfter = 365 -- approx a year after bootsrap
  let lockPeriod = 20 :: Integer
  let baseDeposit = 50 :: Integer

  putStrLn "############ Bootstrap details ##############"
  putStrLn ""

  putStrLn "------------------ Common -------------------"
  putStrLn $ "Network (" ++ show env ++ ") : " ++ show network -- Required for generating addresses
  putStrLn $ "Collector address : " ++ url ++ "/address/" ++ collectionAddress
  putStrLn "---------------------------------------------"
  putStrLn ""

  let refAddress = AFV.referenceAddressBech32 network
  let afvPolicyId = validatorHash' AFV.validator
  putStrLn "----------- 1. Always Fail Validator -----------"
  putStrLn $ "Reference address : " ++ refAddress -- Address the Validator and Minting scripts are sent as reference scripts.
  putStrLn $ "Url               : " ++ url ++ "/address/" ++ refAddress
  putStrLn $ "Own policy ID     : " ++ show afvPolicyId
  putStrLn "------------------- Saving --------------------"
  -- Save script
  AFV.saveScript
  putStrLn "-----------------------------------------------"
  putStrLn ""

  let oref = CNM.generateOutRef txId idx
  let cnftSymbol = CNM.controlNFTCurrencySymbol oref
  let cnftPolicyID = policyHash $ CNM.nftPolicy oref CNM.letters
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
  let collectionPkh = bech32ToPubkeyHash collectionAddress
  let cp = fromMaybe (error "Invalid PubKeyHash.") collectionPkh
  let ct = fromMaybe (error "Invalid Time string.") $ posixTimeFromIso8601 collectionTime
  let tdp = TDV.TimeDepositParams {TDV.dpCollector = cp, TDV.dpCollectionTime = ct}

  let timdedepositValidator = validatorHash' $ TDV.validator tdp
  let receiveAddress = TDV.timeDepositAddressBech32 network tdp
  putStrLn "----------- 3. Time Deposit Validator ----------"
  putStrLn $ "Own addr (deposit to send) : " ++ receiveAddress
  putStrLn $ "Own policy ID              : " ++ show timdedepositValidator
  putStrLn "----------------  Parameters  -----------------"
  putStrLn $ "1. Collector Pkh   : " ++ show collectionPkh
  putStrLn $ "2. Collection time : " ++ collectionTime
  putStrLn "------------------- Saving --------------------"
  -- Save script
  TDV.saveTimeDepositScript cp ct
  putStrLn "-----------------------------------------------"
  putStrLn ""

  let cnftValidator = ATV.valHashBySymbol cnftSymbol

  putStrLn "------- 4. Control NFT (CNFT) Validator --------"
  putStrLn $ "Own policy ID             : " ++ show cnftValidator
  putStrLn $ "Reference address to send : " ++ url ++ "/address/" ++ refAddress
  putStrLn "----------------  Parameters  -----------------"
  putStrLn $ "1. CNFT Currency Symbol : " ++ show cnftSymbol
  putStrLn "!!!IMPORTANT!!!"
  putStrLn "The minting policy id will be included in the"
  putStrLn "CNFT validator datum at initialisation time, as"
  putStrLn "both must cross eference each other."
  putStrLn "------------------- Saving --------------------"
  -- Save script
  ATV.savePolicyScript cnftSymbol
  putStrLn "-----------------------------------------------"
  putStrLn ""

  putStrLn "---------- 5. Adatag Minting Script ----------"
  putStrLn $ "CNFT Currency Symbol      : " ++ show cnftSymbol
  putStrLn $ "Deactivation time : " ++ deactivationTime

  putStrLn "CNFT Validator policy ID  : "
  putStrLn $ "Time Deposit Policy ID    : " ++ show timdedepositValidator
  putStrLn $ "Time Deposit Lock Expiry  : " ++ show deactivationTime
  putStrLn $ "Time Deposit Lock Period  : " ++ show lockPeriod
  putStrLn $ "Time Deposit Deposit Base : " ++ show baseDeposit
  putStrLn $ "Adahandle Policy ID       : " ++ adahandle
  putStrLn "------------------- Saving --------------------"
  putStrLn "-----------------------------------------------"