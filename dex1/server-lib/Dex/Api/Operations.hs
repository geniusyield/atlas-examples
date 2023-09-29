{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

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
  , mShowUtxos
  ) where


import           GeniusYield.Imports
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GeniusYield.Types.UTxO (GYUTxOs)
import Dex.Api.Scripts
import qualified Data.Map.Strict       as Map
import           Dex.OnChain.Dex.Compiled
import Dex.OnChain.Uniswap.Uniswap.Compiled
    ( isUnity,
      unitValue,
      Coin(..),
      PoolState,
      UniswapDatum(..),
      uniswapValidator,
      Uniswap(..), LiquidityPool(..), Amount, Liquidity,
      A, B, U,
      Amount(unAmount), UniswapAction (..)
       )
import qualified Dex.OnChain.Uniswap.Uniswap.Compiled as LQS
import Data.Data (typeOf)
import Plutus.V2.Ledger.Api (POSIXTime, CurrencySymbol, TxOutRef)
import Dex.OnChain.Uniswap.Pool

poolStateTokenName :: GYTokenName
poolStateTokenName = "Pool State"

uniswapValidator' :: Uniswap -> GYValidator 'PlutusV2
uniswapValidator' us = validatorFromPlutus $ uniswapValidator us (poolStateCoin us)

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
    (ref, lps) <- findUniswapFactory us
    gyLogInfo' "" $ printf "Min.showUtxos.2 ref:%s datum:%s" (show ref) (show lps)

    gyLogInfo' "" $ printf "Min.showUtxos.1"
    return mempty

createPool :: GYTxMonad m
               => GYAddress -> Uniswap 
               -> Coin A -> Amount A
               -> Coin B -> Amount B
               -> m (GYTxSkeleton 'PlutusV2)
createPool addr us coinA amountA coinB amountB = do
    gyLogInfo' "" $ printf "createPool 0 createPool us:%s coinA:%s amountA:%s coinB:%s amountB:%s" (show us) (show coinA) (show amountA) (show coinB) (show amountB)
    (ref, lps) <- findUniswapFactory us
    gyLogInfo' "" $ printf "createPool 1 createPool re:%s datum:%s" (show ref) (show lps)
    let liquidity = calculateInitialLiquidity amountA amountB
        lp = LiquidityPool {lpCoinA = coinA, lpCoinB = coinB}
    gyLogInfo' "" $ printf "createPool 2 createPool liquidity:%s lp:%s" (show liquidity) (show lp)
    gyLogInfo' "" $ printf "createPool 3"
    let usInst      = uniswapValidator' us
        usDat1      = Factory $ lp : lps
        usDat2      = Pool lp liquidity
        psC         = poolStateCoin us
        lC          = mkCoin (liquidityCurrency us) $ lpTicker lp
        usVal       = unitValue $ usCoin us
        lpVal       = unitValue psC  <> LQS.valueOf coinA amountA <> LQS.valueOf coinB amountB
    scriptAddr <- uniswapAddress us 

    gyLogInfo' "" $ printf "createPool 4"

    usCVal <- valueFromPlutus' usVal
    lpCVal <- valueFromPlutus' lpVal

    gyLogInfo' "" $ printf "createPool 5"
    lpTokenName <- tokenNameFromPlutus' $ lpTicker lp
    let 
        liquidityPolicy'' = liquidityPolicy' us lpTokenName
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
                    , gyTxOutValue = lpCVal
                    --, gyTxOutDatum = Nothing
                    , gyTxOutDatum = Just (datumFromPlutusData usDat2, GYTxOutUseInlineDatum)
                    , gyTxOutRefS    = Nothing
                    })

                  -- <> skel    
                  <> mustHaveOutput (GYTxOut
                    { gyTxOutAddress = scriptAddr
                    , gyTxOutValue = usCVal
                    --, gyTxOutDatum = Nothing
                    , gyTxOutDatum = Just (datumFromPlutusData usDat1, GYTxOutUseInlineDatum)
                    , gyTxOutRefS    = Nothing
                    })     
                  <> mustMint liquidityPolicy'' unitRedeemer lpTokenName (unAmount liquidity) 
{-

-}  

    gyLogInfo' "" $ printf "createPool 7"
    gyLogInfo' "" $ printf "createPool 7.1 %s" (show txSkeleton)
    --gyLogInfo' "" $ printf "createPool 7.1 Skeleton: %s" txSkeleton
    gyLogInfo' "" $ printf "createPool 8"
    return txSkeleton

findUniswapInstance :: (HasCallStack, GYTxMonad m )
                       => Uniswap -> Coin b 
                       -> (UniswapDatum -> Maybe a) -> m (GYTxOutRef, a)
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
                go ((ref, (_, _, d)) : xs) = do
                    case f d of
                        Nothing -> go xs
                        Just a  -> do
                            return (ref, a)

findUniswapFactory :: GYTxMonad m => Uniswap -> m (GYTxOutRef, [LiquidityPool])
findUniswapFactory us@Uniswap{..} = findUniswapInstance us usCoin $ \case
    Factory lps -> Just lps
    Pool _ _    -> Nothing


findUniswapPool :: (GYTxMonad m, Eq LiquidityPool) => Uniswap -> LiquidityPool -> m (GYTxOutRef, Amount Liquidity)
findUniswapPool us lp = findUniswapInstance us (poolStateCoin us) $ \case
        Pool lp' l
            | lp == lp' -> Just l
        _               -> Nothing


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