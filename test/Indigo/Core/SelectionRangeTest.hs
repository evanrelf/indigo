module Indigo.Core.SelectionRangeTest
  ( module Indigo.Core.SelectionRangeTest
  )
where

import Hedgehog
import Indigo.Core.SelectionRange
import Indigo.Core.PositionTest (genPosition)

hprop_merge_is_commutative :: Property
hprop_merge_is_commutative = property do
  r1 <- forAll genSelectionRange
  r2 <- forAll genSelectionRange
  r1 `merge` r2 === r2 `merge` r1

hprop_merge_is_associative :: Property
hprop_merge_is_associative = property do
  r1 <- forAll genSelectionRange
  r2 <- forAll genSelectionRange
  r3 <- forAll genSelectionRange
  (r1 `merge` r2) `merge` r3 === r1 `merge` (r2 `merge` r3)

genSelectionRange :: Gen SelectionRange
genSelectionRange = fromPositions <$> genPosition <*> genPosition
