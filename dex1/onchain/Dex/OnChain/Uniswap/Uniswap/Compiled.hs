{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Dex.OnChain.Uniswap.Uniswap.Compiled (
    uniswapValidator,
    Uniswap,
    Coin,
    PoolState,
    UniswapDatum (..),
    unitValue,
    isUnity,
) where

import qualified Plutus.V2.Ledger.Api as V2  
import qualified Plutus.V1.Ledger.Scripts

import qualified PlutusTx

import           Dex.OnChain.Uniswap.Types
import           Dex.OnChain.Uniswap.OnChain
import           Dex.OnChain.Uniswap.Pool


-- | Generates validator given params.
uniswapValidator :: Uniswap -> Coin PoolState -> V2.Validator
uniswapValidator us c = V2.mkValidatorScript $
    $$(PlutusTx.compile [|| mkUniswapValidator' ||]) 
    `PlutusTx.applyCode` PlutusTx.liftCode us
    `PlutusTx.applyCode` PlutusTx.liftCode c
