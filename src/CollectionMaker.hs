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
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module CollectionMaker where

import           Ledger               hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)

{-# INLINABLE mkPolicy #-}
mkPolicy :: AssetClass -> BuiltinData -> ScriptContext -> Bool
mkPolicy asset _ ctx =
  mintedAmount < 0
    || traceIfFalse "The CollectionMaker is not present." (nftSum > 0)
  where
    txInfo = scriptContextTxInfo ctx
    txInValues = [txOutValue $ txInInfoResolved txIn | txIn <- txInfoInputs $ scriptContextTxInfo ctx]
    nftValues = [assetClassValueOf val asset | val <- txInValues]
    nftSum = sum nftValues
    mintedAmount = case flattenValue (txInfoMint txInfo) of
        [(cs, _, amt)] | cs == ownCurrencySymbol ctx -> amt
        _                                            -> 0

policy :: AssetClass -> Scripts.MintingPolicy
policy asset =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode asset

curSymbol :: AssetClass -> CurrencySymbol
curSymbol asset = scriptCurrencySymbol $ policy asset
