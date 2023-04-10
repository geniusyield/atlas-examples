module Dex.Api.Operations
  ( mintTestTokens
  , mintTestTokens'
  ) where


import           GeniusYield.Imports
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import Dex.Api.Scripts

mintTestTokens :: GYTxMonad m
               => GYTokenName
               -> Natural
               -> m (GYAssetClass, GYTxSkeleton 'PlutusV2)
mintTestTokens tn amt = do
    -- utxo to base token of.
    utxo <- someUTxO

    let amt'   = toInteger (max 1 amt) -- mint at least 1 token.
        policy = testTokenPolicy amt' (tokenNameToPlutus tn) utxo

    let txSkeleton = mustHaveInput (GYTxIn utxo GYTxInWitnessKey)
                  <> mustMint policy unitRedeemer tn amt'

    return (GYToken (mintingPolicyId policy) tn, txSkeleton)
    
mintTestTokens' :: GYTxMonad m
               => GYTokenName
               -> Natural
               -> m (GYTxSkeleton 'PlutusV2)
mintTestTokens' tn amt = do
    -- utxo to base token of.
    (ass, txSkeleton) <- mintTestTokens tn amt
    return txSkeleton
--mintContract :: GYTxQueryMonad m => GYAddress -> GYValue -> m (GYTxSkeleton 'PlutusV2)
--mintContract ownAddr val = do
--  pkh <- addressToPubKeyHash' ownAddr
--  return $ mustMint
