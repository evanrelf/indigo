{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Core.RangeTest
  ( module Indigo.Core.RangeTest
  )
where

import Hedgehog hiding (Range)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Indigo.Core.Position (Position (..))
import Indigo.Core.PositionTest (genPosition)
import Indigo.Core.Range
import Prelude hiding (flip)

hprop_target_column_greater_than_cursor_column :: Property
hprop_target_column_greater_than_cursor_column = property do
  anchor <- forAll genPosition
  cursor <- forAll genPosition
  targetColumn <- forAll $ Gen.maybe $ Gen.word (Range.linear 0 1000)
  let mRange = setTargetColumn targetColumn $ fromPositions anchor cursor
  whenJust mRange \range ->
    assert $ isTargetColumnGreaterThanCursorColumn range

hprop_flip_forgets_target_column :: Property
hprop_flip_forgets_target_column = property do
  range <- forAll genRange
  if isReduced range then
    flip (flip range) === range
  else
    flip (flip range) === forgetTargetColumn range

hprop_reduce_remembers_target_column :: Property
hprop_reduce_remembers_target_column = property do
  range <- forAll genRange
  targetColumn (reduce range) === targetColumn range

-- TODO: It's not!
-- hprop_merge_is_commutative :: Property
-- hprop_merge_is_commutative = property do
--   r1 <- forAll genRange
--   r2 <- forAll genRange
--   r1 `merge` r2 === r2 `merge` r1

-- TODO: It's not!
-- hprop_merge_is_associative :: Property
-- hprop_merge_is_associative = property do
--   r1 <- forAll genRange
--   r2 <- forAll genRange
--   r3 <- forAll genRange
--   (r1 `merge` r2) `merge` r3 === r1 `merge` (r2 `merge` r3)

genRange :: Gen Range
genRange = do
  anchor <- genPosition
  cursor <- genPosition
  targetColumn <- Gen.maybe $ Gen.word (Range.linear (cursor.column + 1) 1000)
  let mRange = setTargetColumn targetColumn $ fromPositions anchor cursor
  whenNothing mRange do
    fail "`genRange` failed to generate a valid `Range`"
