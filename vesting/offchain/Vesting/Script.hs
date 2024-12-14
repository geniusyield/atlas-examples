module Vesting.Script (
    vestingValidator,
) where

import GeniusYield.Types
import Vesting.OnChain.Vesting.Compiled qualified as OnChain

vestingValidator :: GYPubKeyHash -> GYScript 'PlutusV3
vestingValidator = scriptFromPlutus . OnChain.vestingValidator . pubKeyHashToPlutus
