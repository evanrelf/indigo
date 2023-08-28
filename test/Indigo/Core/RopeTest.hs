module Indigo.Core.RopeTest where

import Hedgehog
import Indigo.Core.Rope
import Indigo.Core.Utilities (unsafeIntToWord)
import Prelude hiding (toText)
import Test.Tasty.HUnit

import qualified Data.Text as Text
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

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
