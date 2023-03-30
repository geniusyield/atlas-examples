module Vesting.Script
    ( vestingValidator
    ) where

import           GeniusYield.Types
import qualified Vesting.OnChain.Vesting.Compiled as OnChain

vestingValidator :: GYPubKeyHash -> GYValidator 'PlutusV2
vestingValidator = validatorFromPlutus . OnChain.vestingValidator . pubKeyHashToPlutus
