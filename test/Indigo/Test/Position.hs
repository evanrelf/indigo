{-# LANGUAGE TemplateHaskell #-}

module Indigo.Test.Position where

import Hedgehog
import Indigo.Position (Position (..))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: Group
tests = $$(discover)

genPosition :: Gen Position
genPosition = do
  line <- Gen.word (Range.linear 0 1000)
  column <- Gen.word (Range.linear 0 1000)
  pure $ Position{ line, column }
