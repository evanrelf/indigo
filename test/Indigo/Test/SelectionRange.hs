{-# LANGUAGE TemplateHaskell #-}

module Indigo.Test.SelectionRange
  ( tests
  , genSelectionRange
  )
where

import Hedgehog
import Indigo.Core.SelectionRange
import Indigo.Test.Position (genPosition)

import qualified Hedgehog.Gen as Gen

tests :: Group
tests = $$(discover)

prop_merge_is_commutative :: Property
prop_merge_is_commutative = property do
  r1 <- forAll genSelectionRange
  r2 <- forAll genSelectionRange
  r1 `merge` r2 === r2 `merge` r1

prop_merge_is_associative :: Property
prop_merge_is_associative = property do
  r1 <- forAll genSelectionRange
  r2 <- forAll genSelectionRange
  r3 <- forAll genSelectionRange
  (r1 `merge` r2) `merge` r3 === r1 `merge` (r2 `merge` r3)

genSelectionRange :: Gen SelectionRange
genSelectionRange = fromPositions <$> genPosition <*> genPosition
