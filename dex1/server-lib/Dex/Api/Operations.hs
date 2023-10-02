{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase #-}

module Dex.Api.Operations
  ( mintTestTokens
  , mintTestTokens'
  , helloChecks
  , mintDatumTokens
  , listsDatum
  , createFactory
  , listFactory
  , listFactory'
  , UniswapDatum(..)
  , listBalance'
  , createPool
  , closePool
  , mShowUtxos
  , pools
  , poolsGY
  , funds
  , remove
  , add
  , swap
  ) where


import           GeniusYield.Imports
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GeniusYield.Types.UTxO (GYUTxOs)
import Dex.Api.Scripts
import qualified Data.Map.Strict       as Map
import           Dex.OnChain.Dex.Compiled
import qualified Dex.OnChain.Uniswap.Uniswap.Compiled as LQS
import Data.Data (typeOf)
import Plutus.V2.Ledger.Api (POSIXTime, CurrencySymbol, TxOutRef)
import Dex.OnChain.Uniswap.Pool
import Dex.OnChain.Uniswap.Types 
import Dex.OnChain.Uniswap.Types as TTA
import Plutus.V1.Ledger.Value (assetClassValue, Value)

--import qualified Data.Type.Equality as LiquidityPool
--import qualified PlutusTx as TT
--import PlutusTx.Prelude as TT2



poolStateTokenName :: GYTokenName
poolStateTokenName = "Pool State"

uniswapValidator' :: Uniswap -> GYValidator 'PlutusV2
uniswapValidator' us = validatorFromPlutus $ LQS.uniswapValidator us (poolStateCoin us)

uniswapAddress :: (HasCallStack, GYTxQueryMonad m) => Uniswap -> m GYAddress
uniswapAddress us = scriptAddress $ uniswapValidator' us 

liquidityCurrency :: Uniswap -> CurrencySymbol
liquidityCurrency us = mintingPolicyCurrencySymbol $ liquidityPolicy' us poolStateTokenName

poolStateCoin :: Uniswap -> Coin PoolState
poolStateCoin us = mkCoin (liquidityCurrency us) (tokenNameToPlutus poolStateTokenName)

factoryCoin :: Uniswap -> Coin U
factoryCoin = mkCoin' . unCoin . usCoin 

createFactory :: GYTxMonad m
               => Uniswap 
               -> m (GYTxSkeleton 'PlutusV2)
createFactory us = do
    let c = factoryCoin us
    scriptAddr <- uniswapAddress us
    unitVal <- valueFromPlutus' $ unitValue c
    let 
        txSkeleton = mustHaveOutput (GYTxOut
                    { gyTxOutAddress = scriptAddr
                    , gyTxOutValue = unitVal
                    --, gyTxOutDatum = Nothing
                    , gyTxOutDatum = Just (datumFromPlutusData (Factory []), GYTxOutUseInlineDatum)
                    , gyTxOutRefS    = Nothing
                    })
    return txSkeleton

mShowUtxos :: GYTxMonad m => Uniswap 
               -> m (GYTxSkeleton 'PlutusV2)
mShowUtxos us = do
    gyLogInfo' "" $ printf "Min.showUtxos.1"
    (ref, v, lps) <- findUniswapFactory us
    gyLogInfo' "" $ printf "Min.showUtxos.2 ref:%s val: %s datum:%s" (show ref) (show v) (show lps)

    gyLogInfo' "" $ printf "Min.showUtxos.1"
    return mempty

createPool :: GYTxMonad m
               => GYAddress -> Uniswap 
               -> Coin A -> Amount A
               -> Coin B -> Amount B
               -> m (GYTxSkeleton 'PlutusV2)
createPool addr us coinA amountA coinB amountB = do
    gyLogInfo' "" $ printf "createPool 0 createPool us:%s coinA:%s amountA:%s coinB:%s amountB:%s" (show us) (show coinA) (show amountA) (show coinB) (show amountB)
    when (unCoin coinA == unCoin coinB) $ error "coins must be different"
    when (amountA <= 0 || amountB <= 0) $ error "amounts must be positive"
    (ref, v, lps) <- findUniswapFactory us
    gyLogInfo' "" $ printf "createPool 1 createPool ref:%s val:%s datum:%s" (show ref) (show v) (show lps)
    let liquidity = calculateInitialLiquidity amountA amountB
        lp = LiquidityPool {lpCoinA = coinA, lpCoinB = coinB}
    gyLogInfo' "" $ printf "createPool 2 createPool liquidity:%s lp:%s" (show liquidity) (show lp)
    gyLogInfo' "" $ printf "createPool 3"
    let usInst      = uniswapValidator' us
        usDat1      = Factory $ lp : lps
        usDat2      = Pool lp liquidity
        usC         = usCoin us
        psC         = poolStateCoin us
        lCur        = liquidityCurrency us
        lC          = mkCoin lCur $ lpTicker lp
    usVal      <- valueFromPlutus' (unitValue usC)
    lpVal      <- valueFromPlutus' (unitValue psC  <> LQS.valueOf coinA amountA <> LQS.valueOf coinB amountB)
    scriptAddr <- uniswapAddress us 

    gyLogInfo' "" $ printf "createPool 4"

    gyLogInfo' "" $ printf "createPool 5"
    lpTokenName <- tokenNameFromPlutus' $ lpTicker lp
    let 
        --liquidityPolicy'' = liquidityPolicy' us lpTokenName
        psLiquidityPolicy = liquidityPolicy' us poolStateTokenName
    gyLogInfo' "" $ printf "createPool 6"
    
    gyLogInfo' "" $ printf "createPool 6.0 %s" (show lpTokenName)
    refs <- utxoRefsAtAddress addr
    uts <- utxosAtAddress addr
    let
        skel = mconcat [mustHaveInput (GYTxIn ut GYTxInWitnessKey) | ut <- refs ]
    gyLogInfo' "" $ printf "createPool 6.1 %s" (show skel)
    gyLogInfo' "" $ printf "createPool 6.2 %s" (show "hello 2")
    gyLogInfo' "" $ printf "createPool 6.3 %s" (show uts)

    let 
        txSkeleton =
                  mustMint psLiquidityPolicy unitRedeemer poolStateTokenName 1
                  <> mustHaveInput (GYTxIn { gyTxInTxOutRef = ref
                                            , gyTxInWitness  = GYTxInWitnessScript
                                                (GYInScript usInst)
                                                (datumFromPlutusData (Factory lps))
                                                (redeemerFromPlutusData (Create lp))
                                            })

                  <> mustHaveOutput (GYTxOut
                    { gyTxOutAddress = scriptAddr
                    , gyTxOutValue = lpVal
                    --, gyTxOutDatum = Nothing
                    , gyTxOutDatum = Just (datumFromPlutusData usDat2, GYTxOutUseInlineDatum)
                    , gyTxOutRefS    = Nothing
                    })

                  -- <> skel    
                  <> mustHaveOutput (GYTxOut
                    { gyTxOutAddress = scriptAddr
                    , gyTxOutValue = usVal
                    --, gyTxOutDatum = Nothing
                    , gyTxOutDatum = Just (datumFromPlutusData usDat1, GYTxOutUseInlineDatum)
                    , gyTxOutRefS    = Nothing
                    })     
                  <> mustMint psLiquidityPolicy unitRedeemer lpTokenName (unAmount liquidity) 
{-

-}  

    gyLogInfo' "" $ printf "createPool 7"
    gyLogInfo' "" $ printf "createPool 7.1 %s" (show txSkeleton)
    --gyLogInfo' "" $ printf "createPool 7.1 Skeleton: %s" txSkeleton
    gyLogInfo' "" $ printf "createPool 8"
    return txSkeleton

closePool :: GYTxMonad m
               => Uniswap
               -> Coin A
               -> Coin B
               -> m (GYTxSkeleton 'PlutusV2)
closePool us coinA coinB = do
    gyLogInfo' "" $ printf "MIN.closePool.1"
    ((oref1, v1, lps), (oref2, v2, lp, liquidity)) <- findUniswapFactoryAndPool us coinA coinB
    gyLogInfo' "" $ printf "MIN.closePool.1.1 %s %s %s" (show oref1) (show v1) (show lps)
    gyLogInfo' "" $ printf "MIN.closePool.1.2 %s %s %s %s" (show oref2) (show v2) (show lp) (show liquidity)
    let usInst      = uniswapValidator' us
        usDat       = Factory $ filter (/= lp) lps
        usC         = usCoin us
        psC         = poolStateCoin us
        lC          = mkCoin (liquidityCurrency us) $ lpTicker lp
        lVal        = redeemerFromPlutusData Close
    usVal       <- valueFromPlutus' (unitValue usC)
    psVal       <- valueFromPlutus' (unitValue psC)
    scriptAddr <- uniswapAddress us 
    gyLogInfo' "" $ printf "MIN.closePool.2"

    lpTokenName <- tokenNameFromPlutus' $ lpTicker lp
    let 
        --liquidityPolicy'' = liquidityPolicy' us lpTokenName
        psLiquidityPolicy = liquidityPolicy' us poolStateTokenName    
    let 
        txSkeleton =
                  mustMint psLiquidityPolicy unitRedeemer poolStateTokenName (negate 1)
                  <> mustMint psLiquidityPolicy unitRedeemer lpTokenName (negate(unAmount liquidity))
                  <> mustHaveInput (GYTxIn { gyTxInTxOutRef = oref1
                                            , gyTxInWitness  = GYTxInWitnessScript
                                                (GYInScript usInst)
                                                (datumFromPlutusData (Factory lps))
                                                lVal
                                            })
                  <> mustHaveInput (GYTxIn { gyTxInTxOutRef = oref2
                                            , gyTxInWitness  = GYTxInWitnessScript
                                                (GYInScript usInst)
                                                (datumFromPlutusData (Pool lp liquidity))
                                                lVal
                                            })
                  <> mustHaveOutput (GYTxOut
                    { gyTxOutAddress = scriptAddr
                    , gyTxOutValue = usVal
                    --, gyTxOutDatum = Nothing
                    , gyTxOutDatum = Just (datumFromPlutusData usDat, GYTxOutUseInlineDatum)
                    , gyTxOutRefS    = Nothing
                    })

    gyLogInfo' "" $ printf "MIN.closePool.3 skeleton %s" (show txSkeleton)

    return txSkeleton

remove :: GYTxMonad m
               => Uniswap
               -> Coin A
               -> Coin B
               -> Amount Liquidity
               -> m (GYTxSkeleton 'PlutusV2)
remove us coinA coinB rpDiff = do
    gyLogInfo' "" $ printf "MIN.remove.1 us:%s" (show us)
    gyLogInfo' "" $ printf "MIN.remove.1.1 conA:%s coinB:%s rpDiff:%s" (show coinA) (show coinB) (show rpDiff)
    ((_, _, _), (oref, v, lp, liquidity)) <- findUniswapFactoryAndPool us coinA coinB
    gyLogInfo' "" $ printf "MIN.remove.1.2 %s %s %s %s" (show oref) (show v) (show lp) (show liquidity)
    when (rpDiff < 1 || rpDiff >= liquidity) $ error "removed liquidity must be positive and less than total liquidity"
    let usInst      = uniswapValidator' us
        usDat       = Pool lp $ liquidity - rpDiff
        psC         = poolStateCoin us
        redeemer    = redeemerFromPlutusData Remove
        v'          = valueToPlutus v
        inA         = amountOf v' coinA
        inB         = amountOf v' coinB
        (outA, outB) = calculateRemoval inA inB liquidity rpDiff
        val          = unitValue psC <> valueOf coinA outA <> valueOf coinB outB
        psLiquidityPolicy = liquidityPolicy' us poolStateTokenName    
    scriptAddr <- uniswapAddress us 
    let 
    val' <- valueFromPlutus' val

    gyLogInfo' "" $ printf "MIN.remove.2.1 coinA:%s coinB:%s" (show coinA) (show coinB)
    gyLogInfo' "" $ printf "MIN.remove.2.2 outA:%s outB:%s" (show outA) (show outB)

    lpTokenName <- tokenNameFromPlutus' $ lpTicker lp
    let 
    let 
        txSkeleton =
                  mustMint psLiquidityPolicy unitRedeemer lpTokenName (negate $ unAmount rpDiff)
                  <> mustHaveInput (GYTxIn { gyTxInTxOutRef = oref
                                            , gyTxInWitness  = GYTxInWitnessScript
                                                (GYInScript usInst)
                                                (datumFromPlutusData (Pool lp liquidity))
                                                redeemer
                                            })
                  <> mustHaveOutput (GYTxOut
                    { gyTxOutAddress = scriptAddr
                    , gyTxOutValue = val'
                    --, gyTxOutDatum = Nothing
                    , gyTxOutDatum = Just (datumFromPlutusData usDat, GYTxOutUseInlineDatum)
                    , gyTxOutRefS    = Nothing
                    })

    gyLogInfo' "" $ printf "MIN.remove.3 skeleton %s" (show txSkeleton)

    return txSkeleton

add :: GYTxMonad m
               => Uniswap
               -> Coin A -> Amount A
               -> Coin B -> Amount B
               -> m (GYTxSkeleton 'PlutusV2)
add us coinA amountA coinB amountB = do
    gyLogInfo' "" $ printf "MIN.add.1 us:%s" (show us)
    gyLogInfo' "" $ printf "MIN.add.1.1 coinA:%s AmountA: %s" (show coinA) (show amountA)
    gyLogInfo' "" $ printf "MIN.add.1.2 coinB:%s AmountB: %s" (show coinB) (show amountB)
    ((_, _, _), (oref, v, lp, liquidity)) <- findUniswapFactoryAndPool us coinA coinB
    gyLogInfo' "" $ printf "MIN.add.1.3 %s %s %s %s" (show oref) (show v) (show lp) (show liquidity)
    let usInst      = uniswapValidator' us
        psC         = poolStateCoin us
        redeemer    = redeemerFromPlutusData Add
        v'          = valueToPlutus v
        oldA        = amountOf v' coinA
        oldB        = amountOf v' coinB
        newA        = oldA + amountA
        newB        = oldB + amountB
        delL        = calculateAdditionalLiquidity oldA oldB liquidity amountA amountB
        usDat       = Pool lp $ liquidity + delL
        val         = unitValue psC <> valueOf coinA newA <> valueOf coinB newB
        psLiquidityPolicy = liquidityPolicy' us poolStateTokenName  
    gyLogInfo' "" $ printf "MIN.add.1.4 delL:%s" (show delL)
    when (delL <= 0) $ error "insufficient liquidity"  
    gyLogInfo' "" $ printf "MIN.add.1.5 oldA:%s oldB:%s newA:%s newB:%s" (show oldA) (show oldB) (show newA) (show newB)

    scriptAddr <- uniswapAddress us 
    val' <- valueFromPlutus' val

    lpTokenName <- tokenNameFromPlutus' $ lpTicker lp
    let 
        txSkeleton =
                     mustMint psLiquidityPolicy unitRedeemer lpTokenName (unAmount delL) 
                  <> mustHaveInput (GYTxIn { gyTxInTxOutRef = oref
                                            , gyTxInWitness  = GYTxInWitnessScript
                                                (GYInScript usInst)
                                                (datumFromPlutusData (Pool lp liquidity))
                                                redeemer
                                            })
                  <> mustHaveOutput (GYTxOut
                    { gyTxOutAddress = scriptAddr
                    , gyTxOutValue = val'
                    --, gyTxOutDatum = Nothing
                    , gyTxOutDatum = Just (datumFromPlutusData usDat, GYTxOutUseInlineDatum)
                    , gyTxOutRefS    = Nothing
                    })

    gyLogInfo' "" $ printf "MIN.add.3 skeleton %s" (show txSkeleton)

    return txSkeleton

swap :: GYTxMonad m
               => Uniswap
               -> Coin A -> Amount A
               -> Coin B -> Amount B
               -> m (GYTxSkeleton 'PlutusV2)
swap us coinA amountA coinB amountB = do
    gyLogInfo' "" $ printf "MIN.swap.1 us:%s" (show us)
    gyLogInfo' "" $ printf "MIN.swap.1.1 coinA:%s AmountA: %s" (show coinA) (show amountA)
    gyLogInfo' "" $ printf "MIN.swap.1.2 coinB:%s AmountB: %s" (show coinB) (show amountB)
    unless (amountA > 0 && amountB == 0 || amountA == 0 && amountB > 0) $ error "exactly one amount must be positive"
    ((_, _, _), (oref, v, lp, liquidity)) <- findUniswapFactoryAndPool us coinA coinB
    gyLogInfo' "" $ printf "MIN.swap.1.3 %s %s %s %s" (show oref) (show v) (show lp) (show liquidity)
    let usInst      = uniswapValidator' us
        psC         = poolStateCoin us
        redeemer    = redeemerFromPlutusData Swap
        usDat       = Pool lp liquidity
        v'          = valueToPlutus v
        oldA        = amountOf v' coinA
        oldB        = amountOf v' coinB
    (newA, newB) <- if amountA > 0 then do
        let outB = Amount $ findSwapA oldA oldB amountA
        when (outB == 0) $ error "no payout"
        return (oldA + amountA, oldB - outB)
                                     else do
        let outA = Amount $ findSwapB oldA oldB amountB
        when (outA == 0) $ error "no payout"
        return (oldA - outA, oldB + amountB)
    let
        val         = unitValue psC <> valueOf coinA newA <> valueOf coinB newB
        psLiquidityPolicy = liquidityPolicy' us poolStateTokenName  
    gyLogInfo' "" $ printf "MIN.swap.1.4 oldA:%s oldB:%s oldProduct:%s" (show oldA) (show oldB) (show (unAmount oldA * unAmount oldB))
    gyLogInfo' "" $ printf "MIN.swap.1.5 newA:%s newB:%s newProduct:%s" (show newA) (show newB) (show (unAmount newA * unAmount newB))

    scriptAddr <- uniswapAddress us 
    val' <- valueFromPlutus' val

    lpTokenName <- tokenNameFromPlutus' $ lpTicker lp
    let 
        txSkeleton =
                     mustHaveInput (GYTxIn { gyTxInTxOutRef = oref
                                            , gyTxInWitness  = GYTxInWitnessScript
                                                (GYInScript usInst)
                                                (datumFromPlutusData (Pool lp liquidity))
                                                redeemer
                                            })
                  <> mustHaveOutput (GYTxOut
                    { gyTxOutAddress = scriptAddr
                    , gyTxOutValue = val'
                    --, gyTxOutDatum = Nothing
                    , gyTxOutDatum = Just (datumFromPlutusData usDat, GYTxOutUseInlineDatum)
                    , gyTxOutRefS    = Nothing
                    })

    gyLogInfo' "" $ printf "MIN.swap.3 skeleton %s" (show txSkeleton)

    return txSkeleton



funds :: GYTxQueryMonad m => GYAddress -> m GYValue
funds addr = do
    balance <- queryBalance addr
    gyLogInfo' "" $ printf "Min.funds.1 balance %s" (show balance)
    return balance


poolsGY :: GYTxQueryMonad m => Uniswap -> m [(GYValue, GYValue)]
poolsGY us = do
    list <- pools us

    pure $ go list
        where 
        go :: [((Coin A, Amount A), (Coin B, Amount B))] -> [(GYValue, GYValue)]
        go [] = []
        go (((aC, aA), (bC, bA)) : xs) = do
            let
                aV = assetClassValue (unCoin aC) (unAmount aA)
                bV = assetClassValue (unCoin bC) (unAmount bA)
            case valueFromPlutus aV of
                Left _ -> go xs
                Right a' -> case valueFromPlutus bV of
                                Left _ -> go xs
                                Right b' -> (a', b') : go xs

pools :: GYTxQueryMonad m => Uniswap -> m [((Coin A, Amount A), (Coin B, Amount B))]
pools us = do
    scriptAddr <- uniswapAddress us
    gyLogInfo' "" $ printf "Min.pools.1 addr %s" (show scriptAddr)
    utxos <- utxosAtAddress scriptAddr
    gyLogInfo' "" $ printf "Min.pools.2 utxos %s" (show utxos)

    let        
        c :: Coin PoolState
        c = poolStateCoin us

        utxos' = filterUTxOs 
                    (\GYUTxO {utxoValue} -> isUnity (valueToPlutus utxoValue) c) 
                    utxos

    gyLogInfo' "" $ printf "Min.pools.3 utxos' %s" (show utxos')    
    datums <- utxosDatums utxos'
    gyLogInfo' "" $ printf "Min.pools.3.2 datums %s" (show datums)    
--     dList <- itoList datums

    pure $ go $ snd <$> Map.toList datums
        where
            go :: [(GYAddress, GYValue, UniswapDatum)] -> [((Coin A, Amount A), (Coin B, Amount B))]
            go [] = []
            go (x@(adr, val, d) : xs) = do
                case d of
                    Pool lp _ -> do
                        let v = valueToPlutus val
                            coinA = lpCoinA lp
                            coinB = lpCoinB lp
                            amtA  = amountOf v coinA
                            amtB  = amountOf v coinB
                            s     = ((coinA, amtA), (coinB, amtB))
                        -- gyLogInfo' "" $ printf "Min.pools.4.Found %s" (show s)
                        let ss = go xs
                        s : ss
                        --return s
                    _ -> go xs
                   -- _ -> go xs

{-
    pure $ go [x | x <- dList]
            where
                go :: [(GYTxOutRef, (GYAddress, GYValue, UniswapDatum))] -> [((Coin A, Amount A), (Coin B, Amount B))]
                go [] = []
                go ((ref, (adr, val, d)) : xs) = do
                    case d of
                        Pool lp _ -> do
                            let v = valueToPlutus val
                                coinA = lpCoinA lp
                                coinB = lpCoinB lp
                                amtA  = amountOf v coinA
                                amtB  = amountOf v coinB
                                s     = ((coinA, amtA), (coinB, amtB))
                            -- gyLogInfo' "" $ printf "Min.pools.4.Found %s" (show s)
                            --ss <- go xs
                            --return $ s : ss
                            return s
                        _ -> go xs

    let utxos' = 
                  [ (utxoValue) 
                  | GYUTxO {utxoValue, utxoOutDatum} <- utxosToList utxos,
                  isUnity (valueToPlutus utxoValue) c,
                    ]
    gyLogInfo' "" $ printf "Min.pools.3 utxos' %s" (show utxos')


    let utxos'' = 
                  [ (utxoValue) 
                  | GYUTxO {utxoValue, utxoOutDatum} <- utxosToList utxos',
                  isUnity (valueToPlutus utxoValue) c
                    ]
    gyLogInfo' "" $ printf "Min.pools.3 utxos' %s" (show utxos'')    

    return mempty

    let utxos' = filterUTxOs 
                    (\GYUTxO {utxoValue} -> isUnity (valueToPlutus utxoValue) c) 
                    utxos
    gyLogInfo' "" $ printf "Min.pools.3 utxos' %s" (show utxos')
-}




findUniswapInstance :: (HasCallStack, GYTxMonad m )
                       => Uniswap -> Coin b 
                       -> (UniswapDatum -> Maybe a) -> m (GYTxOutRef, GYValue, a)
findUniswapInstance us c f = do
    addr <- uniswapAddress us
    gyLogInfo' "" $ printf "findUniswapInstance 1 addr %s" (show addr)
    utxos  <- utxosAtAddress addr
    gyLogInfo' "" $ printf "findUniswapInstance 2 utxos %s" (show utxos)
    let utxos' = filterUTxOs (\GYUTxO {utxoValue} -> isUnity (valueToPlutus utxoValue) c) utxos
    gyLogInfo' "" $ printf "findUniswapInstance 3 utxos %s" (show utxos')
    datums <- utxosDatums utxos'
    go [x | x@(ref, (_, _, d)) <- itoList datums]
            where
                go [] = error "MIN: findUniswapInstance: not found"
                go ((ref, (_, v, d)) : xs) = do
                    case f d of
                        Nothing -> go xs
                        Just a  -> do
                            return (ref, v, a)

findUniswapFactory :: GYTxMonad m => Uniswap -> m (GYTxOutRef, GYValue, [LiquidityPool])
findUniswapFactory us@Uniswap{..} = findUniswapInstance us usCoin $ \case
    Factory lps -> Just lps
    Pool _ _    -> Nothing

findUniswapPool :: GYTxMonad m => Uniswap -> LiquidityPool -> m (GYTxOutRef, GYValue, Amount Liquidity)
findUniswapPool us lp = findUniswapInstance us (poolStateCoin us) $ \case
        Pool lp' l
            | lp == lp' -> Just l
        _               -> Nothing


findUniswapFactoryAndPool :: GYTxMonad m =>
                          Uniswap
                          -> Coin A
                          -> Coin B
                          -> m ( (GYTxOutRef, GYValue, [LiquidityPool])
                               , (GYTxOutRef, GYValue, LiquidityPool, Amount Liquidity)
                               )
findUniswapFactoryAndPool us coinA coinB = do
    (oref1, v1, lps) <- findUniswapFactory us
    case [ lp'
         | lp' <- lps
         , lp' == LiquidityPool coinA coinB
         ] of
        [lp] -> do
            (oref2, v2, a) <- findUniswapPool us lp
            return ( (oref1, v1, lps)
                   , (oref2, v2, lp, a)
                   )
        _    -> error "liquidity pool not found"

findSwapA :: Amount A -> Amount B -> Amount A -> Integer
findSwapA oldA oldB inA
    | ub' <= 1   = 0
    | otherwise  = go 1 ub'
  where
    cs :: Integer -> Bool
    cs outB = checkSwap oldA oldB (oldA + inA) (oldB - Amount outB)

    ub' :: Integer
    ub' = head $ dropWhile cs [2 ^ i | i <- [0 :: Int ..]]

    go :: Integer -> Integer -> Integer
    go lb ub
        | ub == (lb + 1) = lb
        | otherwise      =
      let
        m = div (ub + lb) 2
      in
        if cs m then go m ub else go lb m

findSwapB :: Amount A -> Amount B -> Amount B -> Integer
findSwapB oldA oldB inB = findSwapA (switch oldB) (switch oldA) (switch inB)
  where
    switch = Amount . unAmount


listFactory :: GYTxQueryMonad m 
               => Uniswap 
               -> m [(GYTxOutRef, String)]
listFactory us = do
    addr <- uniswapAddress us 
    gyLogInfo' "" $ printf "listFactory 1 addr %s" (show addr)
    utxos  <- utxosAtAddress addr
    gyLogInfo' "" $ printf "listFactory 2 utxos %s" (show (utxos))
    let utxos2 = utxos -- filterUTxOs (\u@(GYUTxO ref a v mh ms) -> checkDatumX mh) utxos
    utxos' <- utxosDatums utxos2
    -- gyLogInfo' "" $ printf "listsDatum 6 %s" (show (utxos'))
    return
        [ (oref, dat')
        | (oref, (_, val, dat)) <- Map.toList utxos'
        , isUnity (valueToPlutus val) (factoryCoin us)
        , let dat' = printf "%s" (show (dat :: UniswapDatum))
        --, let dat' = dat
        ]

listFactory' :: GYTxQueryMonad m 
               => Uniswap
               -> m [(GYTxOutRef, String)]
listFactory' us = do
    addr <- uniswapAddress us 
    gyLogInfo' "" $ printf "listFactory 1 addr %s" (show addr)
    utxos  <- utxosAtAddress addr
    gyLogInfo' "" $ printf "listFactory 2 utxos %s" (show (utxos))
    let utxos2 = utxos -- filterUTxOs (\u@(GYUTxO ref a v mh ms) -> checkDatumX mh) utxos
    utxos' <- utxosDatums utxos2
    -- gyLogInfo' "" $ printf "listsDatum 6 %s" (show (utxos'))
    return
        [ (oref, dat')
        | (oref, (_, val, dat)) <- Map.toList utxos'
        , isUnity (valueToPlutus val) (factoryCoin us)
        , let dat' = toStringX dat
        --, let dat' = dat
        ]



toStringX :: UniswapDatum -> String
toStringX dat = do
    case dat of
        Factory lps -> printf "%s" (show (dat :: UniswapDatum))
        Pool l a -> printf "%s" (show (dat :: UniswapDatum))



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
    (ass, txSkeleton) <- mintTestTokens tn amt
    return txSkeleton

mintDatumTokens :: GYTxMonad m
               => GYAddress
               -> GYTokenName
               -> Natural
               -> Integer
               -> m (GYTxSkeleton 'PlutusV2)
mintDatumTokens addr tn amt dat = do
    -- utxo to base token of.
    utxo <- someUTxO

    let amt'   = toInteger (max 1 amt) -- mint at least 1 token.
        policy = testTokenPolicy amt' (tokenNameToPlutus tn) utxo
        mtd = MyTestDatum ([1..dat])

    let txSkeleton = mustHaveInput (GYTxIn utxo GYTxInWitnessKey)
                  <> mustMint policy unitRedeemer tn amt' 
                  <> mustHaveOutput (GYTxOut
                    { gyTxOutAddress = addr
                    , gyTxOutValue = mempty
                    , gyTxOutDatum = Just (datumFromPlutusData mtd, GYTxOutUseInlineDatum)
                    -- , gyTxOutDatum = Just (datumFromPlutusData mtd, GYTxOutDontUseInlineDatum)
                    , gyTxOutRefS    = Nothing
                    })
    return txSkeleton

helloChecks :: GYTxQueryMonad m => GYAddress -> m [(GYTxOutRef)]
helloChecks addr = do
    utxos  <- utxoRefsAtAddress addr
    -- datums <- utxosDatums utxos
    return utxos

listBalance' :: GYTxQueryMonad m 
               => [GYAddress]
               -> m GYValue
listBalance' addrs = do
    gyLogInfo' "" $ printf "listBalance 1 %s" (show addrs)
    utxos  <- utxosAtAddresses addrs
    gyLogInfo' "" $ printf "listBalance 2 utxos %s" (show (utxos))
    pure $ foldMapUTxOs utxoValue utxos


listsDatum :: GYTxQueryMonad m => [GYAddress] -> m [(GYTxOutRef, [Integer])]
listsDatum addrs = do
    gyLogInfo' "" $ printf "listsDatum 1 %s" (show addrs)
    slot   <- currentSlot
    gyLogInfo' "" $ printf "listsDatum 2 %s" (show (slot))
    now    <- slotToBeginTime slot
    gyLogInfo' "" $ printf "listsDatum 3 %s" (show (now))
    utxos  <- utxosAtAddresses addrs
    gyLogInfo' "" $ printf "listsDatum 4 %s" (show (utxos))
    let utxos2 = utxos -- filterUTxOs (\u@(GYUTxO ref a v mh ms) -> checkDatumX mh) utxos
    gyLogInfo' "" $ printf "listsDatum 5 %s" (show (utxos2))
    utxos' <- utxosDatums utxos2
    -- gyLogInfo' "" $ printf "listsDatum 6 %s" (show (utxos'))
    return
        [ (oref, dat')
        | (oref, (_, _, dat)) <- Map.toList utxos'
        , let dat' = mtdVals dat
        ]



    {-
    let filt = [u | u@(GYUTxO ref a v mh ms) <- utxosToList utxos, checkDatumX mh]
        filt2 = take 1 . filt
    gyLogInfo' "" $ printf "listsDatum 5 %s" (show (filt))
    utxos' <- utxosDatums utxos
    -- gyLogInfo' "" $ printf "listsDatum 5 %s" (show (utxos'))
    return
        [ (oref, dat')
        | (oref, (_, _, dat)) <- Map.toList utxos'
        , let dat' = mtdVals dat
        ]
    -}


listsDatum1 :: GYTxQueryMonad m => GYAddress -> m [(GYTxOutRef, Maybe [Integer])]
listsDatum1 addr = do
    utxos  <- utxosAtAddress addr

    gyLogInfo' "" $ printf "hello 1 %s" (show (utxos))
    -- no constructor gyLogInfo' "" $ printf "hello 1.2 %s" (show [mh | (ref, (a, v, mh, ms)) <- Map.toList utxos])
    gyLogInfo' "" $ printf "hello 1.1 %s" (show [mh | GYUTxO ref a v mh ms <- utxosToList utxos])
    -- let xy = [case (ref, mh) of { (gtor, god) -> _ }| GYUTxO ref a v mh ms <- utxosToList utxos]
    gyLogInfo' "" $ printf "hello 1.2 %s" (show [(ref, outDatumX mh) | GYUTxO ref a v mh ms <- utxosToList utxos])
    gyLogInfo' "" $ printf "hello 1.3 %s" (show [u | u@(GYUTxO ref a v mh ms) <- utxosToList utxos, checkDatumX mh])
    gyLogInfo' "" $ printf "hello 1.4 %s" (show [u | u@(GYUTxO ref a v mh ms) <- utxosToList utxos, checkDatumX mh])

    {-
    --gyLogInfo' "" $ printf "hello 2 %s" show $ utxosDatums utxos

    let abc = [u | u@(GYUTxO ref a v mh ms) <- utxosToList utxos, checkDatumX mh]
        sing = fmap  . abc
    let filterUtxos = [u | u@(GYUTxO ref a v mh ms) <- utxosToList utxos, checkDatumX mh]
    utxos' <- utxoDatum' $ head . filterUtxos


    utxos' <- utxosDatums utxos
    gyLogDebug' "" $ printf "previous guesses %s" (show "Asdf")
    gyLogInfo' "" $ printf "hello 3 %s" (show ([dat | (oref, (_, _, dat)) <- Map.toList utxos']))
    -}

    pure $
        [(ref, outDatumX mh) | GYUTxO ref a v mh ms <- utxosToList utxos]

{-
dispX :: GYOutDatum -> Maybe [Integer]
dispX GYOutDatumNone     = False
dispX (GYOutDatumHash h) = True
dispX _ = Nothing
-}

checkDatumX :: GYOutDatum -> Bool
checkDatumX GYOutDatumNone     = False
checkDatumX (GYOutDatumHash h) = True
checkDatumX (GYOutDatumInline d) = False

outDatumX :: GYOutDatum -> Maybe [Integer]
outDatumX GYOutDatumNone     = Nothing
outDatumX (GYOutDatumHash h) = Just ([1,2])
outDatumX (GYOutDatumInline d) = Nothing

{-
rt :: [GYOutDatum] -> [[Integer]]
rt dats = do
    case 
listsD :: GYTxQueryMonad m => GYUTxOs -> m [Integer]
listsD utxos = do
        [(case mh of 
            GYOutDatumHash asd -> do
                (_addr, previousValue, dat@(MyTestDatum ints)) <- utxoDatum' u
                Just ints
            _ -> Nothing)
                    | u@(GYUTxO ref a v mh ms) <- utxosToList utxos]
                    --}