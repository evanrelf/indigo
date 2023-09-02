module Indigo.Core.RopeTest
  ( module Indigo.Core.RopeTest
  )
where

import Data.Text qualified as Text
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Indigo.Core.Rope
import Indigo.Core.Utilities (unsafeIntToWord)
import Prelude hiding (empty, lines, toText)
import Test.Tasty.HUnit

hprop_roundtrip :: Property
hprop_roundtrip = property do
  rope <- forAll genRope
  rope === fromText (toText rope)

hprop_ascii_text_roundtrip :: Property
hprop_ascii_text_roundtrip = property do
  text <- forAll $ Gen.text (Range.linear 0 1_000) Gen.ascii
  text === toText (fromText text)

hprop_unicode_text_roundtrip :: Property
hprop_unicode_text_roundtrip = property do
  text <- forAll $ Gen.text (Range.linear 0 1_000) Gen.unicode
  text === toText (fromText text)

hprop_length_chars :: Property
hprop_length_chars = property do
  rope <- forAll genRope
  unsafeIntToWord (Text.length (toText rope)) === lengthChars rope

unit_rope_length_empty :: Assertion
unit_rope_length_empty = do
  lengthChars empty @?= 0
  lengthLines empty @?= 0

unit_rope_length_single_line :: Assertion
unit_rope_length_single_line = do
  lengthChars (fromText "x") @?= 1
  lengthLines (fromText "x") @?= 1

  lengthChars (fromText "\n") @?= 1
  lengthLines (fromText "\n") @?= 1

  lengthChars (fromText "x\n") @?= 2
  lengthLines (fromText "x\n") @?= 1

unit_rope_length_multiple_lines :: Assertion
unit_rope_length_multiple_lines = do
  lengthChars (fromText "x\ny\nz") @?= 5
  lengthLines (fromText "x\ny\nz") @?= 3

  lengthChars (fromText "x\ny\nz\n") @?= 6
  lengthLines (fromText "x\ny\nz\n") @?= 3

unit_rope_char :: Assertion
unit_rope_char = do
  char (CharIndex 0) empty @?= Nothing
  char (CharIndex 1) empty @?= Nothing
  char (CharIndex 0) (fromText "xyz") @?= Just 'x'
  char (CharIndex 1) (fromText "xyz") @?= Just 'y'
  char (CharIndex 2) (fromText "xyz") @?= Just 'z'
  char (CharIndex 3) (fromText "xyz") @?= Nothing

unit_rope_line :: Assertion
unit_rope_line = do
  line (LineIndex 0) empty @?= Nothing
  line (LineIndex 9) (fromText "x\ny\nz") @?= Nothing
  line (LineIndex 0) (fromText "x") @?= Just (fromText "x")
  line (LineIndex 0) (fromText "x\n") @?= Just (fromText "x\n")
  line (LineIndex 0) (fromText "\nx") @?= Just (fromText "\n")
  line (LineIndex 1) (fromText "\nx") @?= Just (fromText "x")

unit_rope_lines :: Assertion
unit_rope_lines = do
  lines empty @?= []
  lines (fromText "x") @?= ["x"]
  lines (fromText "x\n") @?= ["x\n"]
  lines (fromText "x\ny") @?= ["x\n", "y"]
  lines (fromText "x\ny\n") @?= ["x\n", "y\n"]
  lines (fromText "\nx") @?= ["\n", "x"]
  lines (fromText "\nx\n") @?= ["\n", "x\n"]
  lines (fromText "\nx\ny") @?= ["\n", "x\n", "y"]
  lines (fromText "\nx\ny\n") @?= ["\n", "x\n", "y\n"]
  lines (fromText "\n\n") @?= ["\n", "\n"]

genRope :: Gen Rope
genRope = do
  let range = Range.linear 0 1_000
  Gen.recursive Gen.choice
    [ mempty
    , fromText <$> Gen.text range Gen.ascii
    , fromString <$> Gen.string range Gen.ascii
    , fromText <$> Gen.text range Gen.unicode
    , fromString <$> Gen.string range Gen.unicode
    ]
    [ Gen.subterm2 genRope genRope (<>)
    ]
