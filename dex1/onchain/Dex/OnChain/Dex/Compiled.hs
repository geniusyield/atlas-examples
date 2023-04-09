{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Dex.OnChain.Dex.Compiled (
    originalTestTokenPolicy,
) where

import           Plutus.V1.Ledger.Api

import qualified PlutusTx

import           Dex.OnChain.Token

originalTestTokenPolicy
    :: Integer          -- ^ count
    -> TokenName        -- ^ token name (e.g. @GOLD@)
    -> TxOutRef         -- ^ utxo to base token on
    -> MintingPolicy
originalTestTokenPolicy count tn utxo = mkMintingPolicyScript
    $ $$(PlutusTx.compile [|| mkTestTokenPolicy ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode count
    `PlutusTx.applyCode`
     PlutusTx.liftCode tn
    `PlutusTx.applyCode`
     PlutusTx.liftCode utxo