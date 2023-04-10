module Main (main) where


import           Test.Tasty                       (defaultIngredients,
                                                   defaultMainWithIngredients,
                                                   includingOptions, testGroup,
                                                   withResource)
import           Test.Tasty.Ingredients           (Ingredient)

import           GeniusYield.Test.Privnet.Options
import           GeniusYield.Test.Privnet.Setup

import qualified Dex.Tests.Privnet.Tests

ingredients :: [Ingredient]
ingredients = includingOptions optionDescriptions : defaultIngredients

main :: IO ()
main =
  defaultMainWithIngredients ingredients $
  askDbSyncOpts $ \dbSyncOpts ->
  withResource (makeSetup dbSyncOpts) (const mempty) $ \setup ->
    testGroup "Dex" [Dex.Tests.Privnet.Tests.tests setup]
