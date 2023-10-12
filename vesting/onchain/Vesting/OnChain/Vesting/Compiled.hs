{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Vesting.OnChain.Vesting.Compiled
    ( vestingValidator
    ) where

import           PlutusCore.Version      (plcVersion100)
import           PlutusLedgerApi.V2      (PubKeyHash)
import qualified PlutusTx

import           Vesting.OnChain.Vesting (mkWrappedVestingValidator)

-- | Generates validator given parameter.
vestingValidator :: PubKeyHash -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
vestingValidator beneficiary =
    $$(PlutusTx.compile [|| mkWrappedVestingValidator||]) `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 beneficiary
