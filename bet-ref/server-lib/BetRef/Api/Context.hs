module BetRef.Api.Context where

import GeniusYield.GYConfig
import GeniusYield.TxBuilder
import GeniusYield.Types

-- | Our Context.
data Ctx = Ctx
  { ctxCoreCfg :: !GYCoreConfig
  , ctxProviders :: !GYProviders
  }

-- | To run for simple queries, the one which don't requiring building for transaction skeleton.
runQuery :: Ctx -> GYTxQueryMonadIO a -> IO a
runQuery ctx q = do
  let nid = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
  runGYTxQueryMonadIO nid providers q

-- | Tries to build for given skeleton.
runTx ::
  Ctx ->
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxBuilderMonadIO (GYTxSkeleton v) ->
  IO GYTxBody
runTx ctx addrs addr collateral skeleton = do
  let nid = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
  runGYTxBuilderMonadIO
    nid
    providers
    addrs
    addr
    ( collateral
        >>= ( \c ->
                Just
                  ( getTxOutRefHex c
                  , True -- Make this as `False` to not do 5-ada-only check for value in this given UTxO to be used as collateral.
                  )
            )
    )
    (skeleton >>= buildTxBody)