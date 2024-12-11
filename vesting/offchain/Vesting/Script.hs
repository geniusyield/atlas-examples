module Vesting.Script (
    vestingValidator,
) where

import GeniusYield.Types
import Vesting.OnChain.Vesting.Compiled qualified as OnChain

vestingValidator :: GYPubKeyHash -> GYScript 'PlutusV2
vestingValidator = validatorFromPlutus . OnChain.vestingValidator . pubKeyHashToPlutus
