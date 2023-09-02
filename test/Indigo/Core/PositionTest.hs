module Indigo.Core.PositionTest
  ( module Indigo.Core.PositionTest
  )
where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Indigo.Core.Conversion (Conversion (..))
import Indigo.Core.Position
import Indigo.Core.Rope (Rope)
import Indigo.Core.Rope qualified as Rope
import Test.Tasty.HUnit

hprop_rope_conversions_roundtrip :: Property
hprop_rope_conversions_roundtrip = property do
  let rope = Rope.fromText "foo\nbar\nbaz\n"
  position <- forAll $ genPositionInRope rope
  case toCharIndex position rope of
    Invalid -> fail "Position invalid in rope"
    Corrected index ->
      fail $ "Position not in rope, was corrected (" <> show index <> ")"
    Valid index -> fromCharIndex index rope === Valid position

unit_from_char_index_invalid :: Assertion
unit_from_char_index_invalid = do
  let rope = Rope.empty
  let index = 1
  fromCharIndex index rope @?= Invalid

unit_from_char_index_corrected :: Assertion
unit_from_char_index_corrected = do
  do
    let rope = Rope.fromText "foo\nbar"
    let index = 7
    fromCharIndex index rope @?= Corrected Position{ line = 1, column = 2 }

  do
    let rope = Rope.fromText "foo\nbar\n"
    let index = 8
    fromCharIndex index rope @?= Corrected Position{ line = 1, column = 3 }

unit_from_char_index_valid :: Assertion
unit_from_char_index_valid = do
  let rope = Rope.fromText "foo\nbar\n"
  let index = 7
  fromCharIndex index rope @?= Valid Position{ line = 1, column = 3 }

unit_to_char_index_invalid :: Assertion
unit_to_char_index_invalid = do
  let rope = Rope.empty
  let position = Position{ line = 0, column = 0 }
  toCharIndex position rope @?= Invalid

unit_to_char_index_corrected :: Assertion
unit_to_char_index_corrected = do
  do
    let rope = Rope.fromText "foo\nbar"
    let position = Position{ line = 9, column = 0 }
    toCharIndex position rope @?= Corrected 6

  do
    let rope = Rope.fromText "foo\nbar\n"
    let position = Position{ line = 9, column = 0 }
    toCharIndex position rope @?= Corrected 7

unit_to_char_index_valid :: Assertion
unit_to_char_index_valid = do
  let rope = Rope.fromText "foo\nbar\n"
  let position = Position{ line = 1, column = 3 }
  toCharIndex position rope @?= Valid 7

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
