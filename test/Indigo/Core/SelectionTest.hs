module Indigo.Core.SelectionTest where

import Hedgehog hiding (Range)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Indigo.Core.RangeTest (genRange)
import Indigo.Core.Selection

-- prop_ranges_are_sorted :: Property
-- prop_ranges_are_sorted = property do
--   -- isRangesAreSorted
--   undefined

-- prop_ranges_face_same_direction :: Property
-- prop_ranges_face_same_direction = property do
--   -- isRangesFaceSameDirection
--   undefined

-- prop_ranges_not_overlapping :: Property
-- prop_ranges_not_overlapping = property do
--   -- isRangesNotOverlapping
--   undefined

-- prop_rotations_cancel_out :: Property
-- prop_rotations_cancel_out = property do
--   s1 <- forAll genSelection
--   rotateForward (rotateBackward s1) === s1

-- prop_rotations_loop :: Property
-- prop_rotations_loop = property do
--   undefined

-- prop_from_ranges_normalizes :: Property
-- prop_from_ranges_normalizes = property do
--   -- Flipped forward
--   -- Target column forgotten
--   -- Last range is primary
--   undefined

genSelection :: Gen Selection
genSelection = undefined
