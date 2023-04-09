{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module BetRef.OnChain.BetRef.Compiled
    ( betRefValidator
    , BetRefParams (..)
    , OracleAnswerDatum (..)
    , BetRefDatum (..)
    , BetRefAction (..)
    ) where

import           Plutus.V2.Ledger.Api
import qualified PlutusTx

import           BetRef.OnChain.BetRef

-- | Generates validator given params.
betRefValidator :: BetRefParams -> Validator
betRefValidator betRefParams = mkValidatorScript $
    $$(PlutusTx.compile [|| mkBetRefValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode betRefParams
