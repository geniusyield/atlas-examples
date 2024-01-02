module Main (main) where


import           Test.Tasty                       (testGroup, withResource, defaultMain)

import           GeniusYield.Test.Privnet.Setup

import qualified BetRef.Tests.Privnet.Tests

main :: IO ()
main =
  defaultMain $
    withResource makeSetup (const mempty) $ \setup ->
      testGroup "BetRef" [BetRef.Tests.Privnet.Tests.tests setup]
