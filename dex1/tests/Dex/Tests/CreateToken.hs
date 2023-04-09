module Dex.Tests.CreateToken
    ( createTokenTests
    
    ) where

import           Control.Monad.Reader
import           Data.Maybe                     (fromJust)
import qualified Data.Set                       as Set
import           Test.Tasty                     (TestTree, testGroup)

import           Plutus.Model
import           Plutus.Model.Fork.TxExtra

import           Dex.Api.Operations (mintTestTokens)
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

createTokenTests :: TestTree
createTokenTests = testGroup "Create Token Test"
    [ testRun "Balance checks after placing first bet" $ createTrace4 "minToken" 1 ]

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
