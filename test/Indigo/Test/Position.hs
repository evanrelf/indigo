{-# LANGUAGE TemplateHaskell #-}

module Indigo.Test.Position where

import Hedgehog
import Indigo.Core.Position
import Indigo.Core.Rope (Rope)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Indigo.Core.Rope as Rope

tests :: Group
tests = $$(discover)

prop_rope_conversions_roundtrip :: Property
prop_rope_conversions_roundtrip = property do
  let rope = Rope.fromText "foo\nbar\nbaz\n"
  position <- forAll genPosition
  case toRopeIndex position rope of
    Left _ -> empty
    Right index -> fromRopeIndex index rope === Right position

genPosition :: Gen Position
genPosition = do
  line <- Gen.word (Range.linear 0 1000)
  column <- Gen.word (Range.linear 0 1000)
  pure $ Position{ line, column }

genPositionInRope :: Rope -> Gen Position
genPositionInRope rope = do
  let maxLine = undefined
  line <- Gen.word (Range.linear 0 maxLine)
  let maxColumn = undefined
  column <- Gen.word (Range.linear 0 maxColumn)
  pure $ Position{ line, column }

-- | Saturating subtraction
(|-) :: (Bounded a, Ord a, Num a) => a -> a -> a
(|-) x y =
  if x == minBound && y >= 0
    then x
    else x - y

infixl 6 |-
