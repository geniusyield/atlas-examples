module Main (
    main,
) where

import Test.Tasty (defaultMain, testGroup)

import BetRef.Tests.PlaceBet (placeBetTests, placeBetTestsClb)
import BetRef.Tests.TakeBetPot (takeBetPotTests, takeBetPotTestsClb)
import GeniusYield.Test.Privnet.Setup (cardanoDefaultTestnetOptionsConway, withPrivnet)

main :: IO ()
main = do
    defaultMain $
        testGroup
            "Emulator"
            [ placeBetTestsClb
            , takeBetPotTestsClb
            ]
    withPrivnet cardanoDefaultTestnetOptionsConway $ \setup ->
        defaultMain $
            testGroup
                "Privnet"
                [ placeBetTests setup
                , takeBetPotTests setup
                ]
