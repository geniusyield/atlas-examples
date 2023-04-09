module Main
    ( main
    ) where

import           Test.Tasty              (defaultMain, testGroup)

import           Dex.Tests.CreateToken   (createTokenTests)

main :: IO ()
main = defaultMain $ testGroup "Dex" [createTokenTests]
