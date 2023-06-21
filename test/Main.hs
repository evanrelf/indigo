module Main (main) where

import Prelude hiding (group)

import qualified Indigo.Test.Position
import qualified Indigo.Test.Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as Tasty.Hedgehog

main :: IO ()
main =
  Tasty.defaultMain $ Tasty.testGroup "Indigo" $ fmap Tasty.Hedgehog.fromGroup $
    [ Indigo.Test.Position.tests
    , Indigo.Test.Range.tests
    ]
