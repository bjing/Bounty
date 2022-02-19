{--
   Copyright 2021 ₳DAO

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
--}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Bounty where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Credential
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import qualified PlutusTx
import           PlutusTx.Maybe
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)
import           Prelude              (Show)

data Bounty = Bounty
  { bExpiration           :: !POSIXTime,
    bVoters               :: ![PubKeyHash],
    bRequiredVotes        :: !Integer,
    bCollectionMakerClass :: !AssetClass,
    bCollectionToken      :: !AssetClass
  }
  deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''Bounty [('Bounty, 0)]
PlutusTx.makeLift ''Bounty

data Destination = Person PubKeyHash | ScriptH ValidatorHash
  deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed
  ''Destination
  [ ('Person, 0),
    ('ScriptH, 1)
  ]
PlutusTx.makeLift ''Destination

data Collection = Collection
  { cVotes       :: ![PubKeyHash],
    cDestination :: !Destination
  }
  deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''Collection [('Collection, 0)]
PlutusTx.makeLift ''Collection

data BountyDatum = CollectionMaker | CollectionDatum Collection | PotDatum
  deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed
  ''BountyDatum
  [ ('CollectionMaker, 0),
    ('CollectionDatum, 1),
    ('PotDatum, 2)
  ]
PlutusTx.makeLift ''BountyDatum

-- | Return -- TODO we need to make this endpoint a reality as well.
data BountyAction = ApplyVote | CreateCollection Collection | SpendAction
  deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed
  ''BountyAction
  [ ('ApplyVote, 0),
    ('CreateCollection, 1),
    ('SpendAction, 2)
  ]
PlutusTx.makeLift ''BountyAction

data Bountying

instance Scripts.ValidatorTypes Bountying where
  type RedeemerType Bountying = BountyAction
  type DatumType Bountying = BountyDatum

-- Datum Related Functions:

{-# INLINABLE findBountyDatum #-}
findBountyDatum :: TxInfo -> TxOut -> Maybe BountyDatum
findBountyDatum txInfo o = do
  dh <- txOutDatum o
  Datum d <- findDatum dh txInfo
  PlutusTx.fromBuiltinData d

-- Asset Related Functions
{-# INLINABLE collectionMinted #-}
collectionMinted :: ScriptContext -> AssetClass -> Integer
collectionMinted ctx collectionAsset =
  let mintVal = txInfoMint $ scriptContextTxInfo ctx
   in assetClassValueOf mintVal collectionAsset

{-# INLINABLE assetContinues #-}
assetContinues :: [TxOut] -> AssetClass -> Bool
assetContinues continuingOutputs asset =
  sum [assetClassValueOf (txOutValue x) asset | x <- continuingOutputs] > 0

-- Voting Arithmetic Functions
{-# INLINABLE validateCollectionChange #-}
validateCollectionChange :: TxInfo -> [PubKeyHash] -> BountyDatum -> Maybe BountyDatum -> Bool
validateCollectionChange info voters votesBefore maybeDatumAfter = case maybeDatumAfter of
  Just (CollectionDatum c) -> case votesBefore of
    CollectionDatum k -> validateKeyChanges info voters (cVotes k) (cVotes c)
    _                 -> False
  _ -> False

-- This need to reflect false when we don't have enough votes or if the votes are incorrect.
{-# INLINABLE solidCollection #-}
solidCollection :: Bounty -> Collection -> Bool
solidCollection b c =
  let enoughVotes = (bRequiredVotes b) <= (length (cVotes c))
      correctVotes = [a | a <- (cVotes c), elem a (bVoters b)] -- filterVotes (voters b) (votes c)
   in length correctVotes == length (cVotes c)
        && enoughVotes

{-# INLINABLE correctCollection #-}
correctCollection :: TxOut -> Collection -> Bool
correctCollection o c = case (cDestination c) of
  Person pkh -> case (addressCredential (txOutAddress o)) of
    PubKeyCredential opkh -> pkh == opkh
    _                     -> False
  ScriptH vh -> case (addressCredential (txOutAddress o)) of
    ScriptCredential ovh -> vh == ovh
    _                    -> False

-- We need to make sure that the spending path is correct for the PotDatum TxOut TODO
{-# INLINABLE validateUseOfPot #-}
validateUseOfPot :: Bounty -> Maybe TxOut -> Maybe BountyDatum -> Maybe BountyDatum -> Bool
-- validateUseOfPot bounty mPotTxOut mpot mcollection = case mpot of
validateUseOfPot bounty (Just potTxOut) (Just _) (Just (CollectionDatum c)) =
  solidCollection bounty c && correctCollection potTxOut c
validateUseOfPot _ _ _ _ = False

-- This needs to check that
-- - Only valid voters are counted
-- - All new voters have signed the tx.
{-# INLINABLE validateKeyChanges #-}
validateKeyChanges :: TxInfo -> [PubKeyHash] -> [PubKeyHash] -> [PubKeyHash] -> Bool
validateKeyChanges info voters votesBefore votesAfter =
  let newVotes = [a | a <- votesAfter, elem a voters]
      compVal = [a | a <- votesAfter, elem a votesBefore]
   in compVal == votesBefore
        && all (txSignedBy info) newVotes

-- High-Level Functions -- ehh lmao
{-# INLINABLE containsClass #-}
containsClass :: TxOut -> AssetClass -> Bool
containsClass o a = (assetClassValueOf (txOutValue o) a) > 0

{-# INLINABLE findOutputForClass #-}
findOutputForClass :: AssetClass -> [TxOut] -> Maybe TxOut
findOutputForClass asset = find $ \o -> containsClass o asset

{-# INLINABLE containsPot #-}
containsPot :: TxInfo -> TxOut -> Bool
containsPot info o =
  case findBountyDatum info o of
    Just PotDatum -> True
    _             -> False

{-# INLINABLE findOutputPDatum #-}
findOutputPDatum :: TxInfo -> [TxOut] -> Maybe TxOut
findOutputPDatum info = find (containsPot info)

{-# INLINABLE startCollectionDatum #-}
startCollectionDatum :: Maybe BountyDatum -> Bool
startCollectionDatum md = case md of
  Just (CollectionDatum c) -> length (cVotes c) == 0
  _                        -> False

{-# INLINABLE validMakerDatum #-}
validMakerDatum :: Maybe BountyDatum -> Bool
validMakerDatum md = case md of
  Just CollectionMaker -> True
  _                    -> False

{-# INLINABLE validPotDatum #-}
validPotDatum :: Maybe BountyDatum -> Bool
validPotDatum md = case md of
  Just PotDatum -> True
  _             -> False

-- - Collection maker class come and go
-- - CollectionDatum value starts with an empty voter list.
-- -
{-# INLINABLE checkCreateCollection #-}
checkCreateCollection :: ScriptContext -> AssetClass -> AssetClass -> Bool
checkCreateCollection ctx makerAsset collectionAsset =
  let txInfo = scriptContextTxInfo ctx
      txOuts = txInfoOutputs txInfo
      continuingOutputs = getContinuingOutputs ctx
      datumMaker = findOutputForClass makerAsset txOuts >>= findBountyDatum txInfo
      datumBox = findOutputForClass collectionAsset txOuts >>= findBountyDatum txInfo
   in assetContinues continuingOutputs makerAsset
        && assetContinues continuingOutputs collectionAsset
        && (collectionMinted ctx collectionAsset) == 1
        && startCollectionDatum datumBox
        && validMakerDatum datumMaker

-- - For each pubkeyhash being added to the application must have signed.
-- - None of the pubkeyhashes added can be the same as eachother or the values in the list.
{-# INLINABLE checkVoteApplication #-}
checkVoteApplication :: ScriptContext -> AssetClass -> BountyDatum -> [PubKeyHash] -> Bool
checkVoteApplication ctx collectionAsset datum voters =
  let txInfo = scriptContextTxInfo ctx
      txOuts = txInfoOutputs txInfo
      continuingOutputs = getContinuingOutputs ctx
      datumBox = findOutputForClass collectionAsset txOuts >>= findBountyDatum txInfo
   in assetContinues continuingOutputs collectionAsset
        && validateCollectionChange txInfo voters datum datumBox

-- - Are there enough voters in the list
-- - There's only one collectionAsset present as input and it is attached to a valid datum value for usage.
-- - The value attached to the PotDatum is sent to the
{-# INLINABLE checkSpending #-}
checkSpending :: ScriptContext -> Bounty -> Bool
checkSpending ctx bounty =
  let txInfo = scriptContextTxInfo ctx
      txOuts = txInfoOutputs txInfo
      datumBox = findOutputForClass (bCollectionToken bounty) txOuts >>= findBountyDatum txInfo
      potTxOut = findOutputPDatum txInfo txOuts
      potBox = potTxOut >>= findBountyDatum txInfo
   in validateUseOfPot bounty potTxOut potBox datumBox

-- We only can have one CollectionDatum/Token - We need to implement these - definitely.
-- We only can have one

{-# INLINABLE bountyScript #-}
bountyScript :: Bounty -> BountyDatum -> BountyAction -> ScriptContext -> Bool
bountyScript bounty datum action ctx = case datum of
  CollectionMaker -> case action of
    CreateCollection _ -> checkCreateCollection ctx (bCollectionMakerClass bounty) (bCollectionToken bounty)
    _ -> False
  CollectionDatum _ -> case action of
    ApplyVote -> checkVoteApplication ctx (bCollectionMakerClass bounty) datum (bVoters bounty)
    SpendAction -> checkSpending ctx bounty
    _ -> False
  PotDatum -> case action of
    SpendAction -> checkSpending ctx bounty
    _           -> False

bountyValidatorInstance :: Bounty -> Scripts.TypedValidator Bountying
bountyValidatorInstance bounty =
  Scripts.mkTypedValidator @Bountying
    ( $$(PlutusTx.compile [||bountyScript||])
        `PlutusTx.applyCode` PlutusTx.liftCode bounty
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @BountyDatum @BountyAction

bountyValidatorHash :: Bounty -> ValidatorHash
bountyValidatorHash bounty = Scripts.validatorHash (bountyValidatorInstance bounty)

bountyValidatorScript :: Bounty -> Validator
bountyValidatorScript bounty = Scripts.validatorScript (bountyValidatorInstance bounty)

bountyValidatorAddress :: Bounty -> Address
bountyValidatorAddress bounty = Ledger.scriptAddress (bountyValidatorScript bounty)
