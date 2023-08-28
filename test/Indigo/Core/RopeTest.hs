module Indigo.Core.RopeTest where

import Hedgehog
import Indigo.Core.Rope
import Prelude hiding (toText)
import Test.Tasty.HUnit

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

hprop_ascii_text_roundtrip :: Property
hprop_ascii_text_roundtrip = property do
  text <- forAll $ Gen.text (Range.linear 0 1000) Gen.ascii
  text === toText (fromText text)

hprop_unicode_text_roundtrip :: Property
hprop_unicode_text_roundtrip = property do
  text <- forAll $ Gen.text (Range.linear 0 1000) Gen.unicode
  text === toText (fromText text)

hprop_roundtrip :: Property
hprop_roundtrip = property do
  rope <- forAll genRope
  rope === fromText (toText rope)

genRope :: Gen Rope
genRope = do
  Gen.recursive Gen.choice
    [ mempty
    , fromText <$> Gen.text (Range.linear 0 10_000) Gen.ascii
    , fromString <$> Gen.string (Range.linear 0 10_000) Gen.ascii
    , fromText <$> Gen.text (Range.linear 0 10_000) Gen.unicode
    , fromString <$> Gen.string (Range.linear 0 10_000) Gen.unicode
    ]
    [ Gen.subterm2 genRope genRope (<>)
    ]
