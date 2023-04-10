module Dex.Api.Context where

import           GeniusYield.GYConfig
import           GeniusYield.Imports
import           GeniusYield.Transaction
import           GeniusYield.TxBuilder
import           GeniusYield.Types

-- | Our Context.
data Ctx = Ctx
  { ctxCoreCfg   :: !GYCoreConfig
  , ctxProviders :: !GYProviders
  }

-- | To run for simple queries, the one which don't requiring building for transaction skeleton.
runQuery :: Ctx -> GYTxQueryMonadNode a -> IO a
runQuery ctx q = do
  let nid       = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
  runGYTxQueryMonadNode nid providers q

-- | Wraps our skeleton under `Identity` and calls `runTxF`.
runTxI :: Ctx
       -> [GYAddress]     -- ^ user's used addresses
       -> GYAddress       -- ^ user's change address
       -> GYTxOutRefCbor  -- ^ user's collateral
       -> GYTxMonadNode (GYTxSkeleton v)
       -> IO GYTxBody
runTxI = coerce (runTxF @Identity)

-- | Tries to build for given skeletons wrapped under traversable structure.
runTxF :: Traversable t
       => Ctx
       -> [GYAddress]     -- ^ user's used addresses
       -> GYAddress       -- ^ user's change address
       -> GYTxOutRefCbor  -- ^ user's collateral
       -> GYTxMonadNode (t (GYTxSkeleton v))
       -> IO (t GYTxBody)
runTxF = runTxWithStrategyF GYRandomImproveMultiAsset

-- | Create 'TxBody' from a 'GYTxSkeleton', with the specified coin selection strategy.
runTxWithStrategyF :: Traversable t
                   => GYCoinSelectionStrategy
                   -> Ctx
                   -> [GYAddress]
                   -> GYAddress
                   -> GYTxOutRefCbor
                   -> GYTxMonadNode (t (GYTxSkeleton v))
                   -> IO (t GYTxBody)
runTxWithStrategyF cstrat ctx addrs addr collateral skeleton  = do
  let nid       = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
  runGYTxMonadNodeF cstrat nid providers addrs addr (getTxOutRefHex collateral) skeleton

{-



-- | Wraps our skeleton under `Identity` and calls `runTxF`.
runTxI' :: Ctx
       -> [GYAddress]     -- ^ user's used addresses
       -> GYAddress       -- ^ user's change address
       -> GYTxOutRefCbor  -- ^ user's collateral
       -> GYTxMonadNode (GYAssetClass, GYTxSkeleton v)
       -> IO GYTxBody
runTxI' = coerce (runTxF' @Identity)

-- | Tries to build for given skeletons wrapped under traversable structure.
runTxF' :: Traversable t
       => Ctx
       -> [GYAddress]     -- ^ user's used addresses
       -> GYAddress       -- ^ user's change address
       -> GYTxOutRefCbor  -- ^ user's collateral
       -> GYTxMonadNode (t (GYAssetClass, GYTxSkeleton v))
       -> IO (t GYTxBody)
runTxF' = runTxWithStrategyF' GYRandomImproveMultiAsset

-- | Create 'TxBody' from a 'GYTxSkeleton', with the specified coin selection strategy.
runTxWithStrategyF' :: Traversable t
                   => GYCoinSelectionStrategy
                   -> Ctx
                   -> [GYAddress]
                   -> GYAddress
                   -> GYTxOutRefCbor
                   -> GYTxMonadNode (t (GYAssetClass, GYTxSkeleton v))
                   -> IO (t GYTxBody)
runTxWithStrategyF' cstrat ctx addrs addr collateral skeleton  = do
  let nid       = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
  runGYTxMonadNodeF cstrat nid providers addrs addr (getTxOutRefHex collateral) skeleton
-}