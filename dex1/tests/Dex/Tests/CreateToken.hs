module Dex.Tests.CreateToken
    ( createTokenTests
    
    ) where

import           Control.Monad.Reader
import           Data.Maybe                     (fromJust)
import qualified Data.Set                       as Set
import           Test.Tasty                     (TestTree, testGroup)

import           Plutus.Model
import           Plutus.Model.Fork.TxExtra

import           Dex.Api.Operations (mShowUtxos, mintTestTokens, createFactory, createPool, closePool, pools, poolsGY, funds, remove, add, swap)
import           Dex.OnChain.Dex.Compiled

import           GeniusYield.Test.Utils
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import Data.String
import qualified GeniusYield.Types.Logging
import qualified GeniusYield.Types.Value

import Plutus.Model.Fork.Ledger.Scripts
import Data.Either (rights)
import GeniusYield.Imports (printf)
import Dex.Api.Dex (uniswap)
import qualified Dex.OnChain.Uniswap.Types as Script'
import Dex.OnChain.Uniswap.Uniswap.Compiled

createTokenTests :: TestTree
createTokenTests = testGroup "Create Token Test"
    [ testRun "Balance checks after testSwap" $ testSwap "UniswapToken" 1 ]
--    [ testRun "Balance checks after create pool" $ closePool1 "minTokenUS" 1 ]
--    [ testRun "Balance checks after create token" $ createTrace4 "minToken" 1 ]



testSwap :: String -> Natural -> Wallets -> Run ()
testSwap tn noOfTokens ws@Wallets{..} = do
    void $ runWallet w1 $ do 
        addr <- ownAddress
        (b@(ass, tx), diff) <- withBalance (walletName w1) w1 $ do
                    b1@(ass1, skeleton1) <- mintTestTokens tn' noOfTokens
                    logMsg "" GeniusYield.Types.Logging.GYDebug "Min 0" 
                    tx1 <- sendSkeleton skeleton1
                    let us   = uniswap ass1
                    factorySkeleton <- createFactory us
                    factoryTx <- sendSkeleton factorySkeleton
                    poolSkeleton <- createPool addr us 
                        (Script'.Coin fakeGold)
                        (Script'.Amount 500) 
                        (Script'.Coin fakeIron)
                        (Script'.Amount 200)     
                    liftRun $ logInfo $ printf "Min.CreatePool1.1"
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.CreatePool1.1.w1 %s" balw1
                    poolTx <- sendSkeleton poolSkeleton
                    liftRun $ logInfo $ printf "Min.CreatePool2.1"
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.CreatePool2.1.w1 %s" balw1
                    empty <- mShowUtxos us
                    poolsList <- pools us
                    liftRun $ logInfo $ printf "Min.CreatePool3 %s" (show poolsList)
                    liftRun $ logInfo $ printf "Min.CreatePool3.1 %s" (show $ length poolsList)
                    poolsList <- poolsGY us
                    liftRun $ logInfo $ printf "Min.CreatePool4 %s" (show poolsList)
                    liftRun $ logInfo $ printf "Min.CreatePool4.1 %s" (show $ length poolsList)
                    fundsX <- funds addr
                    liftRun $ logInfo $ printf "Min.Funds.1 %s" (show fundsX)

                    liftRun $ logInfo $ printf "Min.Remove.1"
                    removeSkeleton <- remove us 
                        (Script'.Coin fakeGold)
                        (Script'.Coin fakeIron)
                        (Script'.Amount 2)     
                    removeTx <- sendSkeleton removeSkeleton
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.Remove.1.w1 %s" balw1

                    liftRun $ logInfo $ printf "Min.Add.1"
                    addSkeleton <- add us 
                        (Script'.Coin fakeGold)
                        (Script'.Amount 4) 
                        (Script'.Coin fakeIron)
                        (Script'.Amount 2)     
                    addTx <- sendSkeleton addSkeleton
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.Add.1.w1 %s" balw1

                    liftRun $ logInfo $ printf "Min.Swap.1"
                    swapSkeleton <- swap us 
                        (Script'.Coin fakeGold)
                        (Script'.Amount 50) 
                        (Script'.Coin fakeIron)
                        (Script'.Amount 0)     
                    swapTx <- sendSkeleton swapSkeleton
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.Swap.1.w1 %s" balw1


                    return (ass1, tx1)
                    
        liftRun $ logInfo $ printf "Min 1: b %s %s" ass tx
        liftRun $ logInfo $ printf "Min 2: diff %s" diff
        return (b, diff)                                
            where
                tn' :: GYTokenName
                tn' = fromString tn


testAdd :: String -> Natural -> Wallets -> Run ()
testAdd tn noOfTokens ws@Wallets{..} = do
    void $ runWallet w1 $ do 
        addr <- ownAddress
        (b@(ass, tx), diff) <- withBalance (walletName w1) w1 $ do
                    b1@(ass1, skeleton1) <- mintTestTokens tn' noOfTokens
                    logMsg "" GeniusYield.Types.Logging.GYDebug "Min 0" 
                    tx1 <- sendSkeleton skeleton1
                    let us   = uniswap ass1
                    factorySkeleton <- createFactory us
                    factoryTx <- sendSkeleton factorySkeleton
                    poolSkeleton <- createPool addr us 
                        (Script'.Coin fakeGold)
                        (Script'.Amount 4) 
                        (Script'.Coin fakeIron)
                        (Script'.Amount 2)     
                    liftRun $ logInfo $ printf "Min.CreatePool1.1"
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.CreatePool1.1.w1 %s" balw1
                    poolTx <- sendSkeleton poolSkeleton
                    liftRun $ logInfo $ printf "Min.CreatePool2.1"
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.CreatePool2.1.w1 %s" balw1
                    empty <- mShowUtxos us
                    poolsList <- pools us
                    liftRun $ logInfo $ printf "Min.CreatePool3 %s" (show poolsList)
                    liftRun $ logInfo $ printf "Min.CreatePool3.1 %s" (show $ length poolsList)
                    poolsList <- poolsGY us
                    liftRun $ logInfo $ printf "Min.CreatePool4 %s" (show poolsList)
                    liftRun $ logInfo $ printf "Min.CreatePool4.1 %s" (show $ length poolsList)
                    fundsX <- funds addr
                    liftRun $ logInfo $ printf "Min.Funds.1 %s" (show fundsX)

                    liftRun $ logInfo $ printf "Min.Remove.1"
                    removeSkeleton <- remove us 
                        (Script'.Coin fakeGold)
                        (Script'.Coin fakeIron)
                        (Script'.Amount 2)     
                    removeTx <- sendSkeleton removeSkeleton
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.Remove.1.w1 %s" balw1

                    liftRun $ logInfo $ printf "Min.Add.1"
                    addSkeleton <- add us 
                        (Script'.Coin fakeGold)
                        (Script'.Amount 4) 
                        (Script'.Coin fakeIron)
                        (Script'.Amount 2)     
                    addTx <- sendSkeleton addSkeleton
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.Add.1.w1 %s" balw1


                    return (ass1, tx1)
                    
        liftRun $ logInfo $ printf "Min 1: b %s %s" ass tx
        liftRun $ logInfo $ printf "Min 2: diff %s" diff
        return (b, diff)                                
            where
                tn' :: GYTokenName
                tn' = fromString tn



testRemove :: String -> Natural -> Wallets -> Run ()
testRemove tn noOfTokens ws@Wallets{..} = do
    void $ runWallet w1 $ do 
        addr <- ownAddress
        (b@(ass, tx), diff) <- withBalance (walletName w1) w1 $ do
                    b1@(ass1, skeleton1) <- mintTestTokens tn' noOfTokens
                    logMsg "" GeniusYield.Types.Logging.GYDebug "Min 0" 
                    tx1 <- sendSkeleton skeleton1
                    let us   = uniswap ass1
                    factorySkeleton <- createFactory us
                    factoryTx <- sendSkeleton factorySkeleton
                    poolSkeleton <- createPool addr us 
                        (Script'.Coin fakeGold)
                        (Script'.Amount 4) 
                        (Script'.Coin fakeIron)
                        (Script'.Amount 2)     
                    liftRun $ logInfo $ printf "Min.CreatePool1.1"
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.CreatePool1.1.w1 %s" balw1
                    poolTx <- sendSkeleton poolSkeleton
                    liftRun $ logInfo $ printf "Min.CreatePool2.1"
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.CreatePool2.1.w1 %s" balw1
                    empty <- mShowUtxos us
                    poolsList <- pools us
                    liftRun $ logInfo $ printf "Min.CreatePool3 %s" (show poolsList)
                    liftRun $ logInfo $ printf "Min.CreatePool3.1 %s" (show $ length poolsList)
                    poolsList <- poolsGY us
                    liftRun $ logInfo $ printf "Min.CreatePool4 %s" (show poolsList)
                    liftRun $ logInfo $ printf "Min.CreatePool4.1 %s" (show $ length poolsList)
                    fundsX <- funds addr
                    liftRun $ logInfo $ printf "Min.Funds.1 %s" (show fundsX)

                    liftRun $ logInfo $ printf "Min.Remove.1"
                    removeSkeleton <- remove us 
                        (Script'.Coin fakeGold)
                        (Script'.Coin fakeIron)
                        (Script'.Amount 2)     
                    removeTx <- sendSkeleton removeSkeleton
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.Remove.1.w1 %s" balw1

                    return (ass1, tx1)
                    
        liftRun $ logInfo $ printf "Min 1: b %s %s" ass tx
        liftRun $ logInfo $ printf "Min 2: diff %s" diff
        return (b, diff)                                
            where
                tn' :: GYTokenName
                tn' = fromString tn


testFunds :: String -> Natural -> Wallets -> Run ()
testFunds tn noOfTokens ws@Wallets{..} = do
    void $ runWallet w1 $ do 
        addr <- ownAddress
        (b@(ass, tx), diff) <- withBalance (walletName w1) w1 $ do
                    b1@(ass1, skeleton1) <- mintTestTokens tn' noOfTokens
                    logMsg "" GeniusYield.Types.Logging.GYDebug "Min 0" 
                    tx1 <- sendSkeleton skeleton1
                    let us   = uniswap ass1
                    factorySkeleton <- createFactory us
                    factoryTx <- sendSkeleton factorySkeleton
                    poolSkeleton <- createPool addr us 
                        (Script'.Coin fakeGold)
                        (Script'.Amount 4) 
                        (Script'.Coin fakeIron)
                        (Script'.Amount 2)     
                    liftRun $ logInfo $ printf "Min.CreatePool1.1"
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.CreatePool1.1.w1 %s" balw1
                    poolTx <- sendSkeleton poolSkeleton
                    liftRun $ logInfo $ printf "Min.CreatePool2.1"
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.CreatePool2.1.w1 %s" balw1
                    empty <- mShowUtxos us
                    poolsList <- pools us
                    liftRun $ logInfo $ printf "Min.CreatePool3 %s" (show poolsList)
                    liftRun $ logInfo $ printf "Min.CreatePool3.1 %s" (show $ length poolsList)
                    poolsList <- poolsGY us
                    liftRun $ logInfo $ printf "Min.CreatePool4 %s" (show poolsList)
                    liftRun $ logInfo $ printf "Min.CreatePool4.1 %s" (show $ length poolsList)
                    fundsX <- funds addr
                    liftRun $ logInfo $ printf "Min.Funds.1 %s" (show fundsX)
                    return (ass1, tx1)
        liftRun $ logInfo $ printf "Min 1: b %s %s" ass tx
        liftRun $ logInfo $ printf "Min 2: diff %s" diff
        return (b, diff)                                
            where
                tn' :: GYTokenName
                tn' = fromString tn



showPools1 :: String -> Natural -> Wallets -> Run ()
showPools1 tn noOfTokens ws@Wallets{..} = do
    void $ runWallet w1 $ do 
        addr <- ownAddress
        (b@(ass, tx), diff) <- withBalance (walletName w1) w1 $ do
                    b1@(ass1, skeleton1) <- mintTestTokens tn' noOfTokens
                    logMsg "" GeniusYield.Types.Logging.GYDebug "Min 0" 
                    tx1 <- sendSkeleton skeleton1
                    let us   = uniswap ass1
                    factorySkeleton <- createFactory us
                    factoryTx <- sendSkeleton factorySkeleton
                    poolSkeleton <- createPool addr us 
                        (Script'.Coin fakeGold)
                        (Script'.Amount 4) 
                        (Script'.Coin fakeIron)
                        (Script'.Amount 2)     
                    liftRun $ logInfo $ printf "Min.CreatePool1.1"
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.CreatePool1.1.w1 %s" balw1
                    poolTx <- sendSkeleton poolSkeleton
                    liftRun $ logInfo $ printf "Min.CreatePool2.1"
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.CreatePool2.1.w1 %s" balw1
                    empty <- mShowUtxos us
                    poolsList <- pools us
                    liftRun $ logInfo $ printf "Min.CreatePool3 %s" (show poolsList)
                    liftRun $ logInfo $ printf "Min.CreatePool3.1 %s" (show $ length poolsList)
                    poolsList <- poolsGY us
                    liftRun $ logInfo $ printf "Min.CreatePool4 %s" (show poolsList)
                    liftRun $ logInfo $ printf "Min.CreatePool4.1 %s" (show $ length poolsList)
                    return (ass1, tx1)
        liftRun $ logInfo $ printf "Min 1: b %s %s" ass tx
        liftRun $ logInfo $ printf "Min 2: diff %s" diff
        return (b, diff)                                
            where
                tn' :: GYTokenName
                tn' = fromString tn

closePool1 :: String -> Natural -> Wallets -> Run ()
closePool1 tn noOfTokens ws@Wallets{..} = do
    void $ runWallet w1 $ do 
        addr <- ownAddress
        (b@(ass, tx), diff) <- withBalance (walletName w1) w1 $ do
                    b@(ass, tx) <- createTokenRun4 tn' noOfTokens
                    let us   = uniswap ass
                    factorySkeleton <- createFactory us
                    factoryTx <- sendSkeleton factorySkeleton
                    poolSkeleton <- createPool addr us 
                        (Script'.Coin fakeGold)
                        (Script'.Amount 4) 
                        (Script'.Coin fakeIron)
                        (Script'.Amount 2)     
                    liftRun $ logInfo $ printf "Min.CreatePool1.1"
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.CreatePool1.1.w1 %s" balw1
                    poolTx <- sendSkeleton poolSkeleton
                    liftRun $ logInfo $ printf "Min.CreatePool2.1"
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.CreatePool2.1.w1 %s" balw1
                    empty <- mShowUtxos us

                    liftRun $ logInfo $ printf "Min.ClosePool.BeforeCall"
                    closeSkeleton <- closePool us 
                        (Script'.Coin fakeGold)
                        (Script'.Coin fakeIron)
                    closeTx <- sendSkeleton closeSkeleton
                    liftRun $ logInfo $ printf "Min.ClosePool.afterCall"
                    return b
        liftRun $ logInfo $ printf "Min 1: b %s %s" ass tx
        liftRun $ logInfo $ printf "Min 2: diff %s" diff
        return (b, diff)                                
            where
                tn' :: GYTokenName
                tn' = fromString tn


createTokenRun4 :: GYTokenName -> Natural -> GYTxMonadRun (GYAssetClass, GYTxId)
createTokenRun4 tn noOfTokens = do
    (ass, skeleton) <- mintTestTokens tn noOfTokens
    logMsg "" GeniusYield.Types.Logging.GYDebug "Min 0" 
    tx <- sendSkeleton skeleton
    return (ass, tx)

createPool1 :: String -> Natural -> Wallets -> Run ()
createPool1 tn noOfTokens ws@Wallets{..} = do
    void $ runWallet w1 $ do 
        addr <- ownAddress
        (b@(ass, tx), diff) <- withBalance (walletName w1) w1 $ do
                    b@(ass, tx) <- createTokenRun4 tn' noOfTokens
                    let us   = uniswap ass
                    factorySkeleton <- createFactory us
                    factoryTx <- sendSkeleton factorySkeleton
                    poolSkeleton <- createPool addr us 
                        (Script'.Coin fakeGold)
                        (Script'.Amount 4) 
                        (Script'.Coin fakeIron)
                        (Script'.Amount 2)     
                    liftRun $ logInfo $ printf "Min.CreatePool1.1"
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.CreatePool1.1.w1 %s" balw1
                    poolTx <- sendSkeleton poolSkeleton                                         
                    liftRun $ logInfo $ printf "Min.CreatePool2.1"
                    balw1 <- (balance w1)
                    liftRun $ logInfo $ printf "Min.CreatePool2.1.w1 %s" balw1
                    empty <- mShowUtxos us
                    return b
        liftRun $ logInfo $ printf "Min 1: b %s %s" ass tx
        liftRun $ logInfo $ printf "Min 2: diff %s" diff
        return (b, diff)                                
            where
                tn' :: GYTokenName
                tn' = fromString tn

                         
createTokenRun''' :: GYTokenName -> Natural -> GYTxMonadRun GYTxId
createTokenRun''' tn noOfTokens = do
    (ass, skeleton) <- mintTestTokens tn noOfTokens
    logMsg "" GeniusYield.Types.Logging.GYDebug "MinMsg" 
    sendSkeleton skeleton

createTrace4 :: String -> Natural -> Wallets -> Run ()
createTrace4 tn noOfTokens ws@Wallets{..} = do
    void $ runWallet w1 $ do 
        (b, diff) <- withBalance (walletName w1) w1 $ do
                        createTokenRun''' tn' noOfTokens
        if (invalid diff) 
            then fail $ printf "Min: expected balance difference of %s for wallet %s, but the actual difference was %s" b (walletName w1) diff
            else return (b, diff)                                
                            where
                                tn' :: GYTokenName
                                tn' = fromString tn
                                invalid :: GYValue -> Bool
                                invalid diff = 
                                    let 
                                        (actualAda, actualOtherAssets) = valueSplitAda diff
                                    in
                                        actualAda>=0 || (valueTotalAssets actualOtherAssets) /= 1
                                
                                





createTrace''' :: String -> Natural -> Wallets -> Run ()
createTrace''' tn noOfTokens ws@Wallets{..} = do
    void $ runWallet w1 $ do 
        createTokenRun''' tn' noOfTokens
        --logMsg "" GeniusYield.Types.Logging.GYDebug ("MinMsg5: " <> show $ foldr (<>) (valueFromLovelace 419_497) (rights [ valueFromPlutus $ mint'value mint | mint <- mints]))
        withWalletBalancesCheck [w1 := diff] $ do
            createTokenRun''' tn' noOfTokens
            where
                tn' :: GYTokenName
                tn' = fromString tn
                myToken :: GYValue
                myToken = valueSingleton (GYToken "964e562d326a11d362e6e88ae94fa51072e2dd83140dd2e7007aaa28" "minToken") (toInteger noOfTokens)
                fee :: GYValue
                fee = valueFromLovelace 419_497
                diff :: GYValue
                diff = valueNegate fee <> myToken
                     





createTokenRun :: String -> GYTxMonadRun GYTxId
createTokenRun tn = do
    let
        tn' :: GYTokenName
        tn' = fromString tn
    (ass, skeleton) <- mintTestTokens tn' 111_111_111
    logMsg "" GeniusYield.Types.Logging.GYDebug "MinMsg" 
    sendSkeleton skeleton

createTrace :: String -> Wallets -> Run ()
createTrace tn ws@Wallets{..} = do
    void $ runWallet w1 $ do 
        let ctr = createTokenRun tn
        withWalletBalancesCheck [w1 := valueNegate (valueFromLovelace 419_497)] $ do
            ctr

createTokenRun' :: String -> GYTxMonadRun (Tx, GYTxId)
createTokenRun' tn = do
    let
        tn' :: GYTokenName
        tn' = fromString tn
    (ass, skeleton) <- mintTestTokens tn' 111_111_111
    sendSkeleton' skeleton

createTrace' :: String -> Wallets -> Run ()
createTrace' tn ws@Wallets{..} = do
    void $ runWallet w1 $ do 
        (Tx (Extra mints _ _) plutusTxBody, txId) <- createTokenRun' tn
        logMsg "" GeniusYield.Types.Logging.GYDebug ("MinMsg2: " <> show mints)
        logMsg "" GeniusYield.Types.Logging.GYDebug ("MinMsg3: " <> show [ show $ valueFromPlutus $ mint'value mint | mint <- mints])
        logMsg "" GeniusYield.Types.Logging.GYDebug ("MinMsg4: " <> show [ show $ Plutus.Model.Fork.Ledger.Scripts.scriptCurrencySymbol $ mint'policy mint | mint <- mints])
        logMsg "" GeniusYield.Types.Logging.GYDebug ("MinMsg6: " <> show  (rights [ valueFromPlutus $ mint'value mint | mint <- mints]))
        let bal = mconcat $ rights [ valueFromPlutus $ mint'value mint | mint <- mints]
        logMsg "" GeniusYield.Types.Logging.GYDebug ("MinMsg6: " <> show bal)
        let bal2 = valueNegate (valueFromLovelace 419_497) <> bal
        logMsg "" GeniusYield.Types.Logging.GYDebug ("MinMsg7: " <> show bal2)
        --logMsg "" GeniusYield.Types.Logging.GYDebug ("MinMsg5: " <> show $ foldr (<>) (valueFromLovelace 419_497) (rights [ valueFromPlutus $ mint'value mint | mint <- mints]))
        withWalletBalancesCheck [w1 := valueNegate (valueFromLovelace 419_497)] $ do
            --void $ logInfo $ printf "Min6: %v" (rights [ valueFromPlutus $ mint'value mint | mint <- mints])
        --withWalletBalancesCheck [w1 := valueNegate (valueFromLovelace 419_497)] $ do
             -- <> rights [valueFromPlutus $ mint'value mint | mint <- mints]] $ do
            pure txId

createTrace'' :: String -> Wallets -> Run ()
createTrace'' tn ws@Wallets{..} = do
    void $ runWallet w1 $ do 
        --logMsg "" GeniusYield.Types.Logging.GYDebug ("MinMsg5: " <> show $ foldr (<>) (valueFromLovelace 419_497) (rights [ valueFromPlutus $ mint'value mint | mint <- mints]))
        withWalletBalancesCheck [w1 := bal2] $ do
            (Tx (Extra mints _ _) plutusTxBody, txId) <- createTokenRun' tn
            return txId
            where
                --policyId = fromString "964e562d326a11d362e6e88ae94fa51072e2dd83140dd2e7007aaa28" :: GYMintingPolicyId
                --tokenName = fromString "minToken" :: GYTokenName
                myToken = valueSingleton (GYToken "964e562d326a11d362e6e88ae94fa51072e2dd83140dd2e7007aaa28" "minToken") 111_111_111
                --bal = mconcat $ rights [ valueFromPlutus $ mint'value mint | mint <- mints]
                bal2 = valueNegate (valueFromLovelace 419_497) <> myToken
                     
            --void $ logInfo $ printf "Min6: %v" (rights [ valueFromPlutus $ mint'value mint | mint <- mints])
        --withWalletBalancesCheck [w1 := valueNegate (valueFromLovelace 419_497)] $ do
             -- <> rights [valueFromPlutus $ mint'value mint | mint <- mints]] $ do
            --pure txId
