{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Deployer
  ( writeJSON
  , writeValidator
  , writeUnit
  , writeBountyValidator
  ) where

import Bounty
import Cardano.Api
import Cardano.Api.Shelley (PlutusScript(..))
import Codec.Serialise (serialise)
import CollectionMaker (curSymbol)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Ledger
import Plutus.V1.Ledger.Ada
import Plutus.V1.Ledger.Value
import PlutusTx (Data(..))
import qualified PlutusTx

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) =
  ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs) =
  ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs) = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n) = ScriptDataNumber n
dataToScriptData (B bs) = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file =
  LBS.writeFile file .
  encode .
  scriptDataToJson ScriptDataJsonDetailedSchema .
  dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file =
  writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing .
  PlutusScriptSerialised .
  SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()

writeBountyValidator :: IO (Either (FileError ()) ())
writeBountyValidator =
  writeValidator "bounty.plutus" $
  bountyValidatorScript $
  Bounty
  { expiration = 1643812454
  , voters = []
  , requiredVotes = 2
  , collectionMakerClass =
      AssetClass (curSymbol adaAssetClass, "CollectionMaker")
  , collectionToken = adaAssetClass
  }
  where
    adaAssetClass = AssetClass (adaSymbol, "Ada")
