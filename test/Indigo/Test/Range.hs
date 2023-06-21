{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Test.Range where

import Hedgehog hiding (Range)
import Indigo.Position (Position (..))
import Indigo.Range
import Indigo.Test.Position (genPosition)

import Prelude hiding (flip)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: Group
tests = $$(discover)

-- prop_target_column_greater_than_cursor_column :: Property
-- prop_target_column_greater_than_cursor_column = do
--   undefined

prop_flip_forgets_target_column :: Property
prop_flip_forgets_target_column = property do
  r <- forAll genRange
  flip (flip r) === forgetTargetColumn r

-- prop_reduce_remembers_target_column :: Property
-- prop_reduce_remembers_target_column = do
--   undefined

-- TODO: It's not!
-- prop_merge_is_commutative :: Property
-- prop_merge_is_commutative = property do
--   r1 <- forAll genRange
--   r2 <- forAll genRange
--   r1 `merge` r2 === r2 `merge` r1

-- TODO: It's not!
-- prop_merge_is_associative :: Property
-- prop_merge_is_associative = property do
--   r1 <- forAll genRange
--   r2 <- forAll genRange
--   r3 <- forAll genRange
--   (r1 `merge` r2) `merge` r3 === r1 `merge` (r2 `merge` r3)

genRange :: Gen Range
genRange = do
  anchor <- genPosition
  cursor <- genPosition
  targetColumn <- Gen.maybe do
    column <- Gen.word (Range.linear 0 1000)
    guard (cursor.column < column)
    pure column
  case fromRawParts anchor cursor targetColumn of
    Nothing -> fail "`genRange` failed to generate a valid `Range`"
    Just r -> pure r
