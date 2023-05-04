{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Dex.OnChain.Uniswap.Uniswap.Compiled (
    uniswapValidator,
    Uniswap(..),
    Coin(..),
    PoolState,
    UniswapDatum (..),
    unitValue,
    isUnity,
    liquidityPolicy,
    LiquidityPool(..),
    Amount, Liquidity
) where

import qualified Plutus.V2.Ledger.Api as V2  
import qualified Plutus.V1.Ledger.Scripts

import qualified PlutusTx

import Dex.OnChain.Uniswap.Types
    ( isUnity, unitValue, Coin(..), PoolState, Uniswap(..), UniswapDatum(..), Amount, Liquidity, 
    LiquidityPool(..) )
import           Dex.OnChain.Uniswap.OnChain
import           Dex.OnChain.Uniswap.Pool


-- | Generates validator given params.
uniswapValidator :: Uniswap -> Coin PoolState -> V2.Validator
uniswapValidator us c = V2.mkValidatorScript $
    $$(PlutusTx.compile [|| mkUniswapValidator' ||]) 
    `PlutusTx.applyCode` PlutusTx.liftCode us
    `PlutusTx.applyCode` PlutusTx.liftCode c

liquidityPolicy :: Uniswap -> V2.TokenName -> V2.MintingPolicy
liquidityPolicy us poolStateTokenName = V2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| validateLiquidityMinting' ||])
        `PlutusTx.applyCode` PlutusTx.liftCode us
        `PlutusTx.applyCode` PlutusTx.liftCode poolStateTokenName
