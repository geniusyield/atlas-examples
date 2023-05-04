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
      Uniswap(..), LiquidityPool(..), Amount, Liquidity )
import qualified Dex.OnChain.Uniswap.Uniswap.Compiled as LQS
import Data.Data (typeOf)
import Plutus.V1.Ledger.Api (POSIXTime, CurrencySymbol, TxOutRef)

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

factoryCoin :: Uniswap -> Coin PoolState
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

createPool :: GYTxMonad m
               => Uniswap 
               -> GYValue -> GYValue
               -> m (GYTxSkeleton 'PlutusV2)
createPool us a b = do
    (ref, lps) <- findUniswapFactory us
    gyLogInfo' "" $ printf "createPool 1 createPool re:%s datum:%s" (show ref) (show lps)
    let c = poolStateCoin us
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

{-}

    {-
    (ref, d@(lp)) <- findUniswapInstance us (usCoin us) $ \case
        Factory lps -> True
        Pool _ _    -> False
    -}

findUniswapInstance :: (HasCallStack, GYTxMonad m )
                       => Uniswap -> Coin b 
                       -> (UniswapDatum -> Bool) -> m (GYTxOutRef, UniswapDatum)
findUniswapInstance us c f = do
    addr <- uniswapAddress us
    gyLogInfo' "" $ printf "findUniswapInstance 1 addr %s" (show addr)
    utxos  <- utxosAtAddress addr
    gyLogInfo' "" $ printf "findUniswapInstance 2 utxos %s" (show utxos)
    let utxos' = filterUTxOs (\GYUTxO {utxoValue} -> isUnity (valueToPlutus utxoValue) c) utxos
    gyLogInfo' "" $ printf "findUniswapInstance 3 utxos %s" (show utxos')
    utxos'' <- utxosDatums utxos'
    case find (\x@(ref, (_, _, d)) -> f d) $ Map.toList utxos'' of
        Nothing         -> throwError $ GYQueryUTxOException $ GYNoUtxosAtAddress [addr]
        Just x@(ref, (_, _, d)) -> return (ref, d)

findUniswapFactory :: (HasCallStack, GYTxMonad m )
                       => Uniswap -> Coin b 
                       -> (UniswapDatum -> Bool) -> m (GYTxOutRef, [LiquidityPool])
findUniswapFactory  us c f = do
    (ref, d@(lp)) <- findUniswapInstance us (usCoin us) $ \case
        Factory lps -> True
        Pool _ _    -> False
    return lp


    case find (\GYUTxO{..} -> f utxoOutDatum) $ utxosToList utxos'' of
        Nothing         -> fail $ "unable to prepare collateral for wallet " <> show w
        Just GYUTxO{..} -> return utxoRef

    case find (\GYUTxO{..} -> f utxoOutDatum) $ utxosToList utxos'' of
        Nothing         -> fail $ "unable to prepare collateral for wallet " <> show w
        Just GYUTxO{..} -> return utxoRef

                go  [x | x@(ref, (_, _, d)) <- Map.toList utxos'']
                where
                    go [] = throwError "Uniswap instance not found"
                    go ((_, (_, _, d)) : xs) = do
                        case f d of
                            Nothing -> go xs
                            Just a  -> do
                                return a

    go  [x | x@(r, a, v, mh, ms) <- utxosToList utxos'']
  where
    go [] = throwError "Uniswap instance not found"
    go ((r, a, v, mh, ms) : xs) = do
        case f d of
            Nothing -> go xs
            Just a  -> do
                return Nothing --(oref, o, a)   

    return
        [ (oref, dat')
        | (oref, (_, val, dat)) <- Map.toList utxos''
        , let dat' = printf "%s" (show (dat :: UniswapDatum))
        --, let dat' = dat
        ]

    go  [x | x@(r, a, v, mh, ms) <- utxosToList utxos'']
  where
    go [] = throwError "Uniswap instance not found"
    go ((r, a, v, mh, ms) : xs) = do
        d <- getUniswapDatum o
        case f d of
            Nothing -> go xs
            Just a  -> do
                logInfo @String $ printf "found Uniswap instance with datum: %s" (show d)
                return (oref, o, a)   
-}

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