{-# LANGUAGE TemplateHaskell #-}

module Indigo.Test.Position where

import Hedgehog
import Indigo.Core.Position
import Indigo.Core.Rope (Rope)

import qualified Data.Bits as Bits
import qualified Data.Text as Text
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Indigo.Core.Rope as Rope

tests :: Group
tests = $$(discover)

prop_rope_conversions_roundtrip :: Property
prop_rope_conversions_roundtrip = property do
  let rope = Rope.fromText "foo\nbar\nbaz\n"
  position <- forAll $ genPositionInRope rope
  case toRopeIndex position rope of
    Left _ -> fail "Position not in rope, was corrected"
    Right index -> fromRopeIndex index rope === Right position

genPosition :: Gen Position
genPosition = do
  line <- Gen.word (Range.linear 0 1000)
  column <- Gen.word (Range.linear 0 1000)
  pure $ Position{ line, column }

genPositionInRope :: Rope -> Gen Position
genPositionInRope rope = do
  let maxLine = Rope.posLine (Rope.lengthAsPosition rope)
  line <- Gen.word (Range.linear 0 maxLine)
  let maxColumn =
        fromMaybe (error "impossible") do
          int <- Bits.toIntegralSized line
          text <- Rope.lines rope !!? int
          Bits.toIntegralSized $ Text.length text
  column <- Gen.word (Range.linear 0 maxColumn)
  pure $ Position{ line, column }
