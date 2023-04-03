{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Vesting.OnChain.Vesting.Compiled
    ( vestingValidator
    ) where

import           Plutus.V2.Ledger.Api    (PubKeyHash, Validator,
                                          mkValidatorScript)
import qualified PlutusTx

import           Vesting.OnChain.Vesting (mkWrappedVestingValidator)

-- | Generates validator given parameter.
vestingValidator :: PubKeyHash -> Validator
vestingValidator beneficiary = mkValidatorScript $
    $$(PlutusTx.compile [|| mkWrappedVestingValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode beneficiary
