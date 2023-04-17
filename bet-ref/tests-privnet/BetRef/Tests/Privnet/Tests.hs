{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BetRef.Tests.Privnet.Tests (tests) where

import           Test.Tasty                       (TestTree, testGroup)
import           Test.Tasty.HUnit                 (testCaseSteps)

import           BetRef.Api.Operations
import           GeniusYield.Imports

import           BetRef.OnChain.BetRef.Compiled
import           Control.Concurrent               (threadDelay)
import           GeniusYield.Test.Privnet.Asserts
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Test.Privnet.Setup
import           GeniusYield.Types

tests :: IO Setup -> TestTree
tests setup = testGroup "BetRef"
  [
    testCaseSteps "Balance checks & taking pot by closest guesser should pass" $ \info -> withSetup setup info $ \ctx -> do

      -- First step: Construct the parameters and obtain validator from it.
      --
      -- Let's define a new User to represent Oracle (not necessary though)
      oracleUser <- newTempUserCtx ctx (ctxUserF ctx) (valueFromLovelace 20_000_000)
      (currentSlot, slotConfig) <- getSlotAndConfig ctx
      let betUntilSlotDelta = 100
          betRevealSlotDelta = 200
          betUntilTime = slotToBeginTimePure slotConfig (unsafeAdvanceSlot currentSlot betUntilSlotDelta)
          betRevealTime = slotToBeginTimePure slotConfig (unsafeAdvanceSlot currentSlot betRevealSlotDelta)
          brp = BetRefParams (pubKeyHashToPlutus $ userPkh oracleUser) (timeToPlutus betUntilTime) (timeToPlutus betRevealTime) (valueToPlutus $ valueFromLovelace 10_000_000)
          validator = betRefValidator' brp
      validatorAddress <- ctxRunC ctx (ctxUserF ctx) $ betRefAddress brp
      -- Second step: Putting reference script for validator.
      refScript <- addRefScriptCtx ctx (ctxUserF ctx) (validatorToScript validator)
      threadDelay 1_000_000
      -- Third step: Put some bets.
      --
      -- 1st bet.
      txBodyLock <- ctxRunI ctx (ctxUserF ctx) $ placeBet refScript brp (OracleAnswerDatum 1) (valueFromLovelace 10_000_000) (userAddr (ctxUserF ctx)) Nothing
      lockedORef <- findOutput validatorAddress txBodyLock
      void $ submitTx ctx (ctxUserF ctx) txBodyLock
      threadDelay 1_000_000

      --
      -- 2nd bet.
      txBodyLock <- ctxRunI ctx (ctxUser2 ctx) $ placeBet refScript brp (OracleAnswerDatum 2) (valueFromLovelace 20_000_000) (userAddr (ctxUser2 ctx)) (Just lockedORef)
      lockedORef <- findOutput validatorAddress txBodyLock
      void $ submitTx ctx (ctxUser2 ctx) txBodyLock
      threadDelay 1_000_000
      --
      -- 3rd bet.
      txBodyLock <- ctxRunI ctx (ctxUserF ctx) $ placeBet refScript brp (OracleAnswerDatum 3) (valueFromLovelace 35_000_000) (userAddr (ctxUserF ctx)) (Just lockedORef)
      lockedORef <- findOutput validatorAddress txBodyLock
      void $ submitTx ctx (ctxUserF ctx) txBodyLock
      threadDelay 1_000_000

      -- Fourth step, get the bets pot.
      --
      -- Let's first wait for the required amount
      ctxWaitUntilSlot ctx (unsafeAdvanceSlot currentSlot betRevealSlotDelta)  -- here this `currentSlot` is what we obtained sometime ago, the actual current slot has certainly increased a lot by now.
      --
      -- Let's then add for the reference input
      refInputORef <- addRefInputCtx ctx (ctxUserF ctx) True (userAddr oracleUser) (datumFromPlutusData (OracleAnswerDatum 2))
      threadDelay 1_000_000
      --
      -- Balance of `(ctxUser2 ctx)` before unlocking
      balance <- ctxQueryBalance ctx (ctxUser2 ctx)
      --
      -- Unlock operation
      txBodyUnlock <- ctxRunI ctx (ctxUser2 ctx) $ takeBets refScript brp lockedORef (userAddr (ctxUser2 ctx)) refInputORef
      void $ submitTx ctx (ctxUser2 ctx) txBodyUnlock
      threadDelay 1_000_000
      --
      -- Balance of `(ctxUser2 ctx)` after unlocking
      balance' <- ctxQueryBalance ctx (ctxUser2 ctx)
      let diff = valueMinus balance' balance
          adaChange = fst $ valueSplitAda diff  -- should be withing [65 ada - max-fee, 65 ada)
          adaExpectedIncrease = 65_000_000
          maxFee = 1_000_000
      assertBool "(ctxUser2 ctx) ada increase must be b/w [adaExpectedIncrease - maxFee, adaExpectedIncrease)"
           $ adaExpectedIncrease - maxFee <= adaChange && adaChange < adaExpectedIncrease

  , testCaseSteps "Unlocking by not the closest guesser should fail" $ \info -> withSetup setup info $ \ctx -> do

      -- First step: Construct the parameters and obtain validator from it.
      --
      -- Let's define a new User to represent Oracle (not necessary though)
      oracleUser <- newTempUserCtx ctx (ctxUserF ctx) (valueFromLovelace 20_000_000)
      (currentSlot, slotConfig) <- getSlotAndConfig ctx
      let betUntilSlotDelta = 100
          betRevealSlotDelta = 200
          betUntilTime = slotToBeginTimePure slotConfig (unsafeAdvanceSlot currentSlot betUntilSlotDelta)
          betRevealTime = slotToBeginTimePure slotConfig (unsafeAdvanceSlot currentSlot betRevealSlotDelta)
          brp = BetRefParams (pubKeyHashToPlutus $ userPkh oracleUser) (timeToPlutus betUntilTime) (timeToPlutus betRevealTime) (valueToPlutus $ valueFromLovelace 10_000_000)
          validator = betRefValidator' brp
      validatorAddress <- ctxRunC ctx (ctxUserF ctx) $ betRefAddress brp
      -- Second step: Putting reference script for validator.
      refScript <- addRefScriptCtx ctx (ctxUserF ctx) (validatorToScript validator)
      threadDelay 1_000_000
      -- Third step: Put some bets.
      --
      -- 1st bet.
      txBodyLock <- ctxRunI ctx (ctxUserF ctx) $ placeBet refScript brp (OracleAnswerDatum 1) (valueFromLovelace 10_000_000) (userAddr (ctxUserF ctx)) Nothing
      lockedORef <- findOutput validatorAddress txBodyLock
      void $ submitTx ctx (ctxUserF ctx) txBodyLock
      threadDelay 1_000_000

      --
      -- 2nd bet.
      txBodyLock <- ctxRunI ctx (ctxUser2 ctx) $ placeBet refScript brp (OracleAnswerDatum 2) (valueFromLovelace 20_000_000) (userAddr (ctxUser2 ctx)) (Just lockedORef)
      lockedORef <- findOutput validatorAddress txBodyLock
      void $ submitTx ctx (ctxUser2 ctx) txBodyLock
      threadDelay 1_000_000
      --
      -- 3rd bet.
      txBodyLock <- ctxRunI ctx (ctxUserF ctx) $ placeBet refScript brp (OracleAnswerDatum 3) (valueFromLovelace 35_000_000) (userAddr (ctxUserF ctx)) (Just lockedORef)
      lockedORef <- findOutput validatorAddress txBodyLock
      void $ submitTx ctx (ctxUserF ctx) txBodyLock
      threadDelay 1_000_000

      -- Fourth step, get the bets pot.
      --
      -- Let's first wait for the required amount
      ctxWaitUntilSlot ctx (unsafeAdvanceSlot currentSlot betRevealSlotDelta)  -- here this `currentSlot` is what we obtained sometime ago, the actual current slot has certainly increased a lot by now.
      --
      -- Let's then add for the reference input
      refInputORef <- addRefInputCtx ctx (ctxUserF ctx) True (userAddr oracleUser) (datumFromPlutusData (OracleAnswerDatum 2))
      threadDelay 1_000_000
      --
      -- Unlock operation
      -- But this time by wrong guesser
      assertThrown isTxBodyErrorAutoBalance $ ctxRunI ctx (ctxUserF ctx) $ takeBets refScript brp lockedORef (userAddr (ctxUserF ctx)) refInputORef
  ]

getSlotAndConfig :: Ctx -> IO (GYSlot, GYSlotConfig)
getSlotAndConfig ctx = do
  slot <- ctxCurrentSlot ctx
  sc   <- ctxSlotConfig ctx
  return (slot, sc)
