{-# LANGUAGE TemplateHaskell #-}

module Indigo.Test.Position where

import Hedgehog
import Indigo.Core.Conversion (Conversion, Conversion' (..))
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
  position <- forAll $ genPositionInRope rope
  case toRopeIndex position rope of
    Invalid -> fail "Position invalid in rope"
    Corrected _ -> fail "Position not in rope, was corrected"
    Valid index -> fromRopeIndex index rope === Valid position

genPosition :: Gen Position
genPosition = do
  line <- Gen.word (Range.linear 0 1000)
  column <- Gen.word (Range.linear 0 1000)
  pure $ Position{ line, column }

genPositionInRope :: Rope -> Gen Position
genPositionInRope rope = do
  let maxLine = Rope.lengthLines rope
  line <- Gen.word (Range.linear 0 maxLine)
  let maxColumn =
        maybe
          (error "unreachable")
          Rope.lengthChars
          (Rope.line (Rope.LineIndex line) rope)
  column <- Gen.word (Range.linear 0 maxColumn)
  pure $ Position{ line, column }
