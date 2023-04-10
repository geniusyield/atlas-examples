{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dex.Tests.Privnet.Tests (tests) where

import           Test.Tasty                       (TestTree, testGroup)
import           Test.Tasty.HUnit                 (testCaseSteps)

import           Dex.Api.Operations
import           GeniusYield.Imports

import           Dex.OnChain.Dex.Compiled
import           Control.Concurrent               (threadDelay)
import           GeniusYield.Test.Privnet.Asserts
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Test.Privnet.Setup
import           GeniusYield.Types

tests :: IO Setup -> TestTree
tests setup = testGroup "Dex"
  [
    testCaseSteps "Create token should world" $ \info -> withSetup setup info $ \ctx -> do

      (currentSlot, slotConfig) <- getSlotAndConfig ctx
      let
        slotDelta = 10
        tn' :: GYTokenName
        tn' = fromString "minCoin1"         
      balance <- ctxQueryBalance ctx User1

      txBodyLock <- ctxRunI ctx User1 $ mintTestTokens' tn' 1
      --lockedORef <- findOutput (userAddr User1) txBodyLock
      void $ submitTx ctx User1 txBodyLock
      threadDelay 1_000_000

      ctxWaitUntilSlot ctx (unsafeAdvanceSlot currentSlot slotDelta) 

      assertFee txBodyLock 1 1_000_000 

      balance' <- ctxQueryBalance ctx User1
      let
        diff = valueMinus balance' balance
        (actualAda, actualOtherAssets) = valueSplitAda diff
        valid :: GYValue -> Bool
        valid actualOtherAssets = (valueTotalAssets actualOtherAssets) == 1   

      assertBool "check 1 new coin" -- printf "ada: %s other: %s" actualAda actualOtherAssets
           $ valid actualOtherAssets

  ]

getSlotAndConfig :: Ctx -> IO (GYSlot, GYSlotConfig)
getSlotAndConfig ctx = do
  slot <- ctxCurrentSlot ctx
  sc   <- ctxSlotConfig ctx
  return (slot, sc)
