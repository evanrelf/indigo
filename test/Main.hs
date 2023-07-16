module Main (main) where

import Prelude hiding (group)

import qualified Indigo.Test.History
import qualified Indigo.Test.Position
import qualified Indigo.Test.Range
import qualified Indigo.Test.SelectionRange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as Tasty.Hedgehog

main :: IO ()
main =
  Tasty.defaultMain $ Tasty.testGroup "Indigo" $ fmap Tasty.Hedgehog.fromGroup $
    [ Indigo.Test.History.tests
    , Indigo.Test.Position.tests
    , Indigo.Test.Range.tests
    , Indigo.Test.SelectionRange.tests
    ]
