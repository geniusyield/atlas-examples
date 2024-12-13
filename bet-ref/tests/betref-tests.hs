module Main (
    main,
) where

import Test.Tasty (defaultMain, testGroup)

import BetRef.Tests.PlaceBet (placeBetTests, placeBetTestsClb)
import BetRef.Tests.TakeBetPot (takeBetPotTests, takeBetPotTestsClb)
import GeniusYield.Test.Privnet.Setup (cardanoDefaultTestnetOptionsConway, withPrivnet)

main :: IO ()
main = do
    withPrivnet cardanoDefaultTestnetOptionsConway $ \setup ->
        defaultMain $
            testGroup
                "BetRef"
                [ testGroup
                    "Emulator - CLB"
                    [ placeBetTestsClb
                    , takeBetPotTestsClb
                    ]
                , testGroup
                    "Privnet"
                    [ placeBetTests setup
                    , takeBetPotTests setup
                    ]
                ]
