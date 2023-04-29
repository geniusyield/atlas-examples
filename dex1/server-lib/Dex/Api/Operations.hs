{-# LANGUAGE TemplateHaskell            #-}

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
  ) where


import           GeniusYield.Imports
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GeniusYield.Types.UTxO (GYUTxOs)
import Dex.Api.Scripts
import qualified Data.Map.Strict       as Map
import           Dex.OnChain.Dex.Compiled
import           Dex.OnChain.Uniswap.Uniswap.Compiled
import Data.Data (typeOf)
import Plutus.V1.Ledger.Api (POSIXTime)

uniswapValidator' :: Uniswap -> Coin PoolState -> GYValidator 'PlutusV2
uniswapValidator' us c = validatorFromPlutus $ uniswapValidator us c

uniswapAddress :: (HasCallStack, GYTxQueryMonad m) => Uniswap -> Coin PoolState -> m GYAddress
uniswapAddress us c = scriptAddress $ uniswapValidator' us c

createFactory :: GYTxMonad m
               => Uniswap -> Coin PoolState
               -> m (GYTxSkeleton 'PlutusV2)
createFactory us c = do
    scriptAddr <- uniswapAddress us c
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

listFactory :: GYTxQueryMonad m 
               => Uniswap -> Coin PoolState
               -> m [(GYTxOutRef, String)]
listFactory us c = do
    addr <- uniswapAddress us c
    gyLogInfo' "" $ printf "listFactory 1 addr %s" (show addr)
    utxos  <- utxosAtAddress addr
    gyLogInfo' "" $ printf "listFactory 2 utxos %s" (show (utxos))
    let utxos2 = utxos -- filterUTxOs (\u@(GYUTxO ref a v mh ms) -> checkDatumX mh) utxos
    utxos' <- utxosDatums utxos2
    -- gyLogInfo' "" $ printf "listsDatum 6 %s" (show (utxos'))
    return
        [ (oref, dat')
        | (oref, (_, val, dat)) <- Map.toList utxos'
        , isUnity (valueToPlutus val) c
        , let dat' = printf "%s" (show (dat :: UniswapDatum))
        --, let dat' = dat
        ]

listFactory' :: GYTxQueryMonad m 
               => Uniswap -> Coin PoolState
               -> m [(GYTxOutRef, String)]
listFactory' us c = do
    addr <- uniswapAddress us c
    gyLogInfo' "" $ printf "listFactory 1 addr %s" (show addr)
    utxos  <- utxosAtAddress addr
    gyLogInfo' "" $ printf "listFactory 2 utxos %s" (show (utxos))
    let utxos2 = utxos -- filterUTxOs (\u@(GYUTxO ref a v mh ms) -> checkDatumX mh) utxos
    utxos' <- utxosDatums utxos2
    -- gyLogInfo' "" $ printf "listsDatum 6 %s" (show (utxos'))
    return
        [ (oref, dat')
        | (oref, (_, val, dat)) <- Map.toList utxos'
        , isUnity (valueToPlutus val) c
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