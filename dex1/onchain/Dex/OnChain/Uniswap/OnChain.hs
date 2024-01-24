{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-}

module Dex.OnChain.Uniswap.OnChain
    ( mkUniswapValidator
    , mkUniswapValidator'
    , validateLiquidityMinting
    , validateLiquidityMinting'
    ) where

import Data.Void (Void)
import Dex.OnChain.Uniswap.Pool (calculateAdditionalLiquidity, calculateInitialLiquidity, calculateRemoval,
                                      checkSwap, lpTicker)
import Dex.OnChain.Uniswap.Types
import Plutus.V2.Ledger.Api (Datum (Datum), DatumHash, OutputDatum (..), ScriptContext (..), TokenName,
                             TxInInfo (txInInfoResolved), TxInfo (txInfoInputs, txInfoMint),
                             TxOut (txOutDatum, txOutValue), Value, UnsafeFromData(unsafeFromBuiltinData), ScriptPurpose (Minting))
import qualified Plutus.V2.Ledger.Contexts as V2
import qualified PlutusTx
import PlutusTx.Prelude
import Plutus.V1.Ledger.Value (AssetClass(..), symbols, assetClass, flattenValue, CurrencySymbol, assetClassValueOf)
import PlutusTx.Builtins.Class (stringToBuiltinString)



{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (V2.findOwnInput ctx)

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINABLE validateSwap #-}
-- | We check the swap is valid through 'checkSwap', and otherwise just make
-- sure that the pool token is passed through.
validateSwap :: LiquidityPool -> Coin PoolState -> ScriptContext -> Bool
validateSwap LiquidityPool{..} c ctx =
    checkSwap oldA oldB newA newB                                                       &&
    traceIfFalse "expected pool state token to be present in input" (isUnity inVal c)   &&
    traceIfFalse "expected pool state token to be present in output" (isUnity outVal c) &&
    traceIfFalse "did not expect Uniswap minting" noUniswapMinting
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxInInfo
    ownInput = findOwnInput' ctx

    ownOutput :: TxOut
    ownOutput = case [ o
                     | o <- V2.getContinuingOutputs ctx
                     , txOutDatum o == (snd $ V2.ownHashes ctx)
                     ] of
        [o] -> o
        _   -> traceError "expected exactly one output to the same liquidity pool"

    oldA = amountA inVal
    oldB = amountB inVal
    newA = amountA outVal
    newB = amountB outVal

    amountA v = amountOf v lpCoinA
    amountB v = amountOf v lpCoinB

    inVal, outVal :: Value
    inVal  = valueWithin ownInput
    outVal = txOutValue ownOutput

    noUniswapMinting :: Bool
    noUniswapMinting =
      let
        AssetClass (cs, _) = unCoin c
        minted             = txInfoMint info
      in
        notElem cs $ symbols minted

{-# INLINABLE validateCreate #-}
-- | Ths validates the creation of a liquidity pool to exchange coins. In order to be
-- valid,
--
--  1,2. We need to be dealing with the Uniswap coin,
--  3. We have to exchanging different coins,
--  4. The pool can't already exist,
--  5. The pool needs a single value as output,
--  6. The liquidity amount needs to be as-determined by 'calculateInitialLiquidity'
--      (i.e. the amount from the Uniswap V2 paper).
--  7,8. We need to be exchanging more than zero of each kind of coin.
--  9. It should output a pool with the determined properties
validateCreate :: Uniswap
               -> Coin PoolState
               -> [LiquidityPool]
               -> LiquidityPool
               -> ScriptContext
               -> Bool
validateCreate Uniswap{..} c lps lp@LiquidityPool{..} ctx =
    traceIfFalse "Min VC 0: Uniswap coin not present" (isUnity (valueWithin $ findOwnInput' ctx) usCoin)     && -- 1.
    traceIfFalse "Min VC 1: " (unCoin lpCoinA /= unCoin lpCoinB)                                                             && -- 3.
    traceIfFalse "Min VC 2: " (notElem lp lps)                                                                                 && -- 4.
    traceIfFalse "Min VC 3: " (isUnity minted c) &&
    traceIfFalse "Min VC 4: " (amountOf minted liquidityCoin' == liquidity)                                                  && -- 6.
    traceIfFalse "Min VC 5: " (outA > 0)                                                                                     && -- 7.
    traceIfFalse "Min VC 6: " (outB > 0)         
  where
    poolOutput :: TxOut
    poolOutput = case [o | o <- V2.getContinuingOutputs ctx, isUnity (txOutValue o) c] of
        [o] -> o
        _   -> traceError "expected exactly one pool output"

    outA      = amountOf (txOutValue poolOutput) lpCoinA
    outB      = amountOf (txOutValue poolOutput) lpCoinB
    liquidity = calculateInitialLiquidity outA outB

    minted :: Value
    minted = txInfoMint $ scriptContextTxInfo ctx

    liquidityCoin' :: Coin Liquidity
    liquidityCoin' = let AssetClass (cs,_) = unCoin c in mkCoin cs $ lpTicker lp

{-
    --traceError "MIN vc 0: expected exactly one pool output"
    --traceIfFalse "MIN: validateCreate: fail" False &&
    traceIfFalse "Uniswap coin not present" (isUnity (valueWithin $ findOwnInput' ctx) usCoin)     && -- 1.
    (unCoin lpCoinA /= unCoin lpCoinB)                                                             && -- 3.
    notElem lp lps                                                                                 && -- 4.
    isUnity minted c                                                                               && -- 5.
    (amountOf minted liquidityCoin' == liquidity)                                                  && -- 6.
    (outA > 0)                                                                                     && -- 7.
    (outB > 0)         
  where
    poolOutput :: TxOut
    poolOutput = case [o | o <- V2.getContinuingOutputs ctx, isUnity (txOutValue o) c] of
        [o] -> o
        _   -> traceError "expected exactly one pool output"

    outA      = amountOf (txOutValue poolOutput) lpCoinA
    outB      = amountOf (txOutValue poolOutput) lpCoinB
    liquidity = calculateInitialLiquidity outA outB

    minted :: Value
    minted = txInfoMint $ scriptContextTxInfo ctx

    liquidityCoin' :: Coin Liquidity
    liquidityCoin' = let AssetClass (cs,_) = unCoin c in mkCoin cs $ lpTicker lp
-}


{-# INLINABLE validateCloseFactory #-}
-- | See 'Plutus.Contracts.Uniswap.OffChain.close'.
validateCloseFactory :: Uniswap -> Coin PoolState -> [LiquidityPool] -> ScriptContext -> Bool
validateCloseFactory Uniswap{..} c lps ctx =
    traceIfFalse "Uniswap coin not present" (isUnity (valueWithin $ findOwnInput' ctx) usCoin)                        && -- 1.
    traceIfFalse "Min.VCF.poolVal incorrect" poolVal && -- 2.
    traceIfFalse "Min.VCF.liqVal incorrect" liqVal && -- 2.
    traceIfFalse "Min.VCF.liqVal2 incorrect" liqVal2 && -- 2.
    traceIfFalse "Min.VCF.liqVal22 incorrect" liqVal22 && -- 2.
    -- traceIfFalse liqVal4 liqVal3 && -- 2.
    traceIfFalse "Min.VCF.liqVal23 incorrect" (ac c == ac lC) && -- 2.
    traceIfFalse "Min.VCF.liqVal24 incorrect" liqVal24 && -- 2.
    traceIfFalse "Min.VCF.liqVal3 incorrect" liqVal3 && -- 2.
    traceIfFalse "MIN.VCF.wrong mint value"        (txInfoMint info == negate (unitValue c <>  valueOf lC (snd lpLiquidity))) -- 2.
--    traceIfFalse "wrong mint value"        (txInfoMint info == negate (unitValue c <>  valueOf lC (snd lpLiquidity))) -- 2.

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    mints :: Value
    mints = txInfoMint info

    poolVal :: Bool
    poolVal = assetClassValueOf mints (unCoin c) == -1

    liqVal :: Bool
    liqVal = unAmount (snd lpLiquidity) == 3

    liqVal2 :: Bool
    liqVal2 = foldr (||) False  [s == ac c | s <- symbols $ txInfoMint info]

    liqVal22 :: Bool
    liqVal22 = foldr (||) False  [s == ac lC | s <- symbols $ txInfoMint info]

    ac :: Coin b -> CurrencySymbol
    ac x = let AssetClass (cs, _) = unCoin x in cs

    liqVal24 :: Bool
    liqVal24 = (length . symbols $ txInfoMint info) == 2

    liqVal3 :: Bool
    liqVal3 = x== -3
      where x = assetClassValueOf mints (unCoin lC)

    --liqVal4 :: BuiltinString
    --liqVal4 = stringToBuiltinString $ show (assetClassValueOf mints (unCoin lC))




    poolInput :: TxInInfo
    poolInput = case [ i
                     | i <- txInfoInputs info
                     , isUnity (valueWithin i) c
                     ] of
        [i] -> i -- traceError "MIN:VCF.PI"
        _   -> traceError "expected exactly one pool input"

    lpLiquidity :: (LiquidityPool, Amount Liquidity)
    lpLiquidity = case txOutDatum . txInInfoResolved $ poolInput of
        NoOutputDatum         -> traceError "pool input witness missing"
        OutputDatumHash dh    -> findPoolDatum info dh
        OutputDatum (Datum d) -> case PlutusTx.unsafeFromBuiltinData d of
                                  (Pool lp a) -> (lp, a)
                                  _           -> traceError "error decoding data"

    lC :: Coin Liquidity
    lC  = let AssetClass (cs, _) = unCoin c in mkCoin cs (lpTicker $ fst lpLiquidity)

{-# INLINABLE validateClosePool #-}
-- | See 'Plutus.Contracts.Uniswap.OffChain.close'.
validateClosePool :: Uniswap -> ScriptContext -> Bool
validateClosePool us ctx = hasFactoryInput
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasFactoryInput :: Bool
    hasFactoryInput =
        traceIfFalse "Uniswap factory input expected" $
        isUnity (V2.valueSpent info) (usCoin us)

{-# INLINABLE validateRemove #-}
-- | See 'Plutus.Contracts.Uniswap.OffChain.remove'.
validateRemove :: Coin PoolState -> LiquidityPool -> Amount Liquidity -> ScriptContext -> Bool
validateRemove c lp liquidity ctx =
    traceIfFalse "zero removal"                        (diff > 0)                                     &&
    traceIfFalse "removal of too much liquidity"       (diff < liquidity)                             &&
    traceIfFalse "pool state coin missing"             (isUnity inVal c)                              &&
    traceIfFalse "wrong liquidity pool output"         (fst lpLiquidity == lp)                        &&
    traceIfFalse "pool state coin missing from output" (isUnity outVal c)                             &&
    traceIfFalse "liquidity tokens not burnt"          (txInfoMint info == negate (valueOf lC diff))  &&
    traceIfFalse "non-positive liquidity"              (outA > 0 && outB > 0)                         &&
    traceIfFalse "removal of invalid amount of tokens" (outA' == outA && outB' == outB)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxInInfo
    ownInput = findOwnInput' ctx

    output :: TxOut
    output = case V2.getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one Uniswap output"

    inVal, outVal :: Value
    inVal  = valueWithin ownInput
    outVal = txOutValue output

    lpLiquidity :: (LiquidityPool, Amount Liquidity)
    lpLiquidity = case txOutDatum output of
        NoOutputDatum         -> traceError "pool output witness missing"
        OutputDatumHash dh    -> findPoolDatum info dh
        OutputDatum (Datum d) -> case PlutusTx.unsafeFromBuiltinData d of
                                  (Pool lp' a) -> (lp', a)
                                  _            -> traceError "error decoding data"

    lC :: Coin Liquidity
    lC = let AssetClass (cs, _) = unCoin c in mkCoin cs (lpTicker lp)

    diff         = liquidity - snd lpLiquidity
    inA          = amountOf inVal $ lpCoinA lp
    inB          = amountOf inVal $ lpCoinB lp
    outA'        = amountOf outVal $ lpCoinA lp
    outB'        = amountOf outVal $ lpCoinB lp
    (outA, outB) = calculateRemoval inA inB liquidity diff

{-# INLINABLE validateAdd #-}
-- | See 'Plutus.Contracts.Uniswap.OffChain.add'.
validateAdd :: Coin PoolState -> LiquidityPool -> Amount Liquidity -> ScriptContext -> Bool
validateAdd c lp liquidity ctx =
    traceIfFalse "pool stake token missing from input"          (isUnity inVal c)                                                    &&
    traceIfFalse "output pool for same liquidity pair expected" (lp == fst outDatum)                                                 &&
    traceIfFalse "must not remove tokens"                       (delA >= 0 && delB >= 0)                                             &&
    traceIfFalse "insufficient liquidity"                       (delL >= 0)                                                          &&
    traceIfFalse "wrong amount of liquidity tokens"             (delL == calculateAdditionalLiquidity oldA oldB liquidity delA delB) &&
    traceIfFalse "wrong amount of liquidity tokens minted"      (txInfoMint info == valueOf lC delL)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxInInfo
    ownInput = findOwnInput' ctx

    ownOutput :: TxOut
    ownOutput = case [ o
                     | o <- V2.getContinuingOutputs ctx
                     , isUnity (txOutValue o) c
                     ] of
        [o] -> o
        _   -> traceError "expected exactly on pool output"

    outDatum :: (LiquidityPool, Amount Liquidity)
    outDatum = case txOutDatum ownOutput of
        NoOutputDatum         -> traceError "pool output datum/hash not found"
        OutputDatumHash dh    -> findPoolDatum info dh
        OutputDatum (Datum d) -> case PlutusTx.unsafeFromBuiltinData d of
                                  (Pool lp' a) -> (lp', a)
                                  _            -> traceError "error decoding data"

    inVal, outVal :: Value
    inVal  = valueWithin ownInput
    outVal = txOutValue ownOutput

    oldA = amountOf inVal aC
    oldB = amountOf inVal bC
    delA = amountOf outVal aC - oldA
    delB = amountOf outVal bC - oldB
    delL = snd outDatum - liquidity

    aC = lpCoinA lp
    bC = lpCoinB lp

    lC :: Coin Liquidity
    lC = let AssetClass (cs, _) = unCoin c in mkCoin cs $ lpTicker lp

{-# INLINABLE findPoolDatum #-}
findPoolDatum :: TxInfo -> DatumHash -> (LiquidityPool, Amount Liquidity)
findPoolDatum info h = case V2.findDatum h info of
    Just (Datum d) -> case PlutusTx.unsafeFromBuiltinData d of
        (Pool lp a) -> (lp, a)
        _           -> traceError "error decoding data"
    _              -> traceError "pool input datum not found"

{-# INLINABLE mkUniswapValidator' #-}
mkUniswapValidator' :: Uniswap
                   -> Coin PoolState
                   -> BuiltinData
                   -> BuiltinData
                   -> BuiltinData
                   -> ()
mkUniswapValidator' us c usd' usr' ctx'
  | mkUniswapValidator us c (unsafeFromBuiltinData usd') (unsafeFromBuiltinData usr') (unsafeFromBuiltinData ctx') = ()
  | otherwise = error ()

{-# INLINABLE mkUniswapValidator #-}
mkUniswapValidator :: Uniswap
                   -> Coin PoolState
                   -> UniswapDatum
                   -> UniswapAction
                   -> ScriptContext
                   -> Bool
mkUniswapValidator us c (Factory lps) (Create lp) ctx = validateCreate us c lps lp ctx
mkUniswapValidator _  c (Pool lp _)   Swap        ctx = validateSwap lp c ctx
mkUniswapValidator us c (Factory lps) Close       ctx = validateCloseFactory us c lps ctx
mkUniswapValidator us _ (Pool _  _)   Close       ctx = validateClosePool us ctx
mkUniswapValidator _  c (Pool lp a)   Remove      ctx = validateRemove c lp a ctx
mkUniswapValidator _  c (Pool lp a)   Add         ctx = validateAdd c lp a ctx
mkUniswapValidator _  _ _             _           _   = False

{-# INLINABLE validateLiquidityMinting' #-}
validateLiquidityMinting' :: Uniswap -> TokenName -> BuiltinData -> BuiltinData -> ()
validateLiquidityMinting' us tn rd' ctx'
  | validateLiquidityMinting us tn (unsafeFromBuiltinData rd') (unsafeFromBuiltinData ctx') = ()
  | otherwise = error ()

{-# INLINABLE validateLiquidityMinting #-}
validateLiquidityMinting :: Uniswap -> TokenName -> () -> ScriptContext -> Bool
validateLiquidityMinting Uniswap{..} tn _ ctx
  =
    case [ i
         | i <- txInfoInputs $ scriptContextTxInfo ctx
         , let v = valueWithin i
         , isUnity v usCoin || isUnity v lpC
         --  isUnity v lpC
          --         , isUnity v usCoin || isPSCUnity v
         ] of
    [_]    -> True
    [_, _] -> True
    _      -> traceError "MIN: pool state minting without Uniswap input"

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    isPSCUnity :: Value -> Bool
    isPSCUnity v = foldr (||) False [isUnity v (lpCX cur) | a@(cur, m, n) <- flattenValue $ txInfoMint info]
--    isPSCUnity v = foldr (||) False [isUnity v (lpCX cur) | a@(cur, m, n) <- flattenValue $ txInfoMint info]

    --[(cur', tn', amt'), (cur'', tn'', amt'')] = flattenValue $ txInfoMint info

    lpCX :: CurrencySymbol -> Coin PoolState
    lpCX cur = Coin $ assetClass cur tn

    lpC :: Coin PoolState
    lpC = mkCoin (ownCurrencySymbol ctx) tn
--    lpC = mkCoin (V2.ownCurrencySymbol ctx) tn

    -- isPSCUnity2 :: Value -> Bool
    -- isPSCUnity2 = 

    ownCurrencySymbol :: ScriptContext -> CurrencySymbol
    ownCurrencySymbol ScriptContext{scriptContextPurpose=Minting cs} = cs
    ownCurrencySymbol _                                              = traceError "Lh" -- "Can't get currency symbol of the current validator script"


 {-
  = 
    traceIfFalse "MIN.VLM.>0: " (length (flattenValue (txInfoMint info)) > 0) && 
    traceIfFalse "MIN.VLM.>1: " (length (flattenValue (txInfoMint info)) > 1) && 
    traceIfFalse "MIN.VLM.>2: " (length (flattenValue (txInfoMint info)) > 2) 
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    True
    case [ i
         | i <- txInfoInputs $ scriptContextTxInfo ctx
         , let v = valueWithin i
         , isUnity v usCoin || isUnity v lpC
         ] of
    [_]    -> True
    [_, _] -> True
    _      -> traceError "MIN: pool state minting without Uniswap input"

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    [(cur', tn', amt')] = flattenValue $ txInfoMint info

    lpC :: Coin PoolState
    lpC = Coin $ assetClass cur' tn
-}
--  True
    --let
--    traceError "MIN 0: pool state minting without Uniswap input" 
--      x = txInfoInputs $ scriptContextTxInfo ctx
--  traceError "MIN 0: pool state minting without Uniswap input"  
--  traceIfFalse "MIN: vlm: throw" False 
--    where 
--      x :: BuiltinString
--      x = length (txInfoInputs $ scriptContextTxInfo ctx)
--  traceIfFalse "MIN: vlm: throw" False 
  --length (txInfoInputs $ scriptContextTxInfo ctx) > 1
{-
    case [ i
         | i <- txInfoInputs $ scriptContextTxInfo ctx
         , let v = valueWithin i
         , isUnity v usCoin || isUnity v lpC
         ] of
    [_]    -> True
    [_, _] -> True
    _      -> traceError "MIN: pool state minting without Uniswap input"
  where
    lpC :: Coin PoolState
    lpC = mkCoin (V2.ownCurrencySymbol ctx) tn
-}