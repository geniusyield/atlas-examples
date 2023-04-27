{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Dex.OnChain.Dex.Compiled (
    originalTestTokenPolicy,
    MyTestDatum (..),
    mkCoin,
    mkCoin',
) where

import qualified Plutus.V2.Ledger.Api   as V2
import qualified Plutus.V1.Ledger.Scripts

import qualified PlutusTx

import           Dex.OnChain.Token

originalTestTokenPolicy
    :: Integer          -- ^ count
    -> V2.TokenName        -- ^ token name (e.g. @GOLD@)
    -> V2.TxOutRef         -- ^ utxo to base token on
    -> V2.MintingPolicy
originalTestTokenPolicy count tn utxo = V2.mkMintingPolicyScript
    $ $$(PlutusTx.compile [|| mkTestTokenPolicy ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode count
    `PlutusTx.applyCode`
     PlutusTx.liftCode tn
    `PlutusTx.applyCode`
     PlutusTx.liftCode utxo