module Vesting.Utils
    ( findCollateral
    ) where

import           GeniusYield.TxBuilder
import           GeniusYield.Types     (GYAddress, GYNetworkId, GYProviders,
                                        GYTxOutRef)

findCollateral :: GYNetworkId -> GYProviders -> GYAddress -> IO GYTxOutRef
findCollateral nid providers addr = do
    m <- runGYTxQueryMonadNode nid providers $ getCollateral' addr 5_000_000
    case m of
        Just (oref, _) -> return oref
        Nothing        -> fail "Error: no collateral found\n"
