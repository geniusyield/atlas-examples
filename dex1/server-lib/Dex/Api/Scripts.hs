module Dex.Api.Scripts (
    testTokenPolicy,
    liquidityPolicy,
    liquidityPolicy',
    Uniswap(..),
) where

import           Plutus.V2.Ledger.Api

import           Dex.OnChain.Dex.Compiled (originalTestTokenPolicy)
import           GeniusYield.Types
import Dex.OnChain.Uniswap.Uniswap.Compiled
    ( Uniswap(..), liquidityPolicy )

testTokenPolicy
    :: Integer           -- ^ count
    -> TokenName         -- ^ token name (e.g. @GOLD@)
    -> GYTxOutRef        -- ^ utxo to base token on
    -> GYMintingPolicy 'PlutusV2
testTokenPolicy count tn utxo =
    mintingPolicyFromPlutus  @'PlutusV2
        $ originalTestTokenPolicy count tn (txOutRefToPlutus utxo)

liquidityPolicy'
    :: Uniswap          
    -> GYTokenName        
    -> GYMintingPolicy 'PlutusV2
liquidityPolicy' us tn =
    mintingPolicyFromPlutus  @'PlutusV2
        $ liquidityPolicy us (tokenNameToPlutus tn)