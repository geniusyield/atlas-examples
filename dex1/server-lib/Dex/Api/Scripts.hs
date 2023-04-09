module Dex.Api.Scripts (
    testTokenPolicy,
) where

import           Plutus.V1.Ledger.Api

import           Dex.OnChain.Dex.Compiled (originalTestTokenPolicy)
import           GeniusYield.Types

testTokenPolicy
    :: Integer           -- ^ count
    -> TokenName         -- ^ token name (e.g. @GOLD@)
    -> GYTxOutRef        -- ^ utxo to base token on
    -> GYMintingPolicy 'PlutusV1
testTokenPolicy count tn utxo =
    mintingPolicyFromPlutus  @'PlutusV1
        $ originalTestTokenPolicy count tn (txOutRefToPlutus utxo)