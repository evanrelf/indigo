{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Core.Selection
  ( Selection

    -- * Create
  , fromRange

    -- * Query
  , ranges
  , primary
  , secondaries
  , direction
  , isForward
  , isBackward

    -- * Modify
  , forgetTargetColumns
  , flip
  , flipForward
  , flipBackward
  , rotateForward
  , rotateBackward

    -- * Consume
  , toRanges

    -- * Internal
  , isValid
  )
where

import Data.Default.Class (Default (..))
import Data.IntervalMap.Generic.Strict (IntervalMap)
import Data.IntervalMap.Generic.Strict qualified as IntervalMap
import Data.List.NonEmpty qualified as NonEmpty
import Indigo.Core.Direction (Direction (..))
import Indigo.Core.Range (Range)
import Indigo.Core.Range qualified as Range
import Indigo.Core.SelectionRange (SelectionRange)
import Indigo.Core.SelectionRange qualified as SelectionRange
import Prelude hiding (flip)

data Selection = Selection
  { primary :: !(SelectionRange, Maybe Word)
  , secondaries :: !(IntervalMap SelectionRange (NonEmpty (Maybe Word)))
  , direction :: !Direction
  }
  deriving stock (Show, Eq)

instance Default Selection where
  def :: Selection
  def =
    Selection
      { primary = (def, Nothing)
      , secondaries = IntervalMap.empty
      , direction = Forward
      }

fromRange :: Range -> Selection
fromRange range =
  Selection
    { primary = (rangeToSelectionRange range, Range.targetColumn range)
    , secondaries = IntervalMap.empty
    , direction = if Range.isForward range then Forward else Backward
    }

ranges :: Selection -> NonEmpty (SelectionRange, Maybe Word)
ranges selection = do
  let (selectionRange, targetColumn) = selection.primary
  let ranges = insert' selectionRange targetColumn selection.secondaries
  (selectionRange, targetColumns) <-
    IntervalMap.toAscList ranges
    & nonEmpty
    & fromMaybe (error "impossible, we just inserted an element")
  targetColumn <- targetColumns
  pure (selectionRange, targetColumn)

primary :: Selection -> (SelectionRange, Maybe Word)
primary selection = selection.primary

secondaries :: Selection -> [(SelectionRange, Maybe Word)]
secondaries selection = do
  (selectionRange, targetColumns) <- IntervalMap.toAscList selection.secondaries
  targetColumn <- toList targetColumns
  pure (selectionRange, targetColumn)

direction :: Selection -> Direction
direction selection = selection.direction

isForward :: Selection -> Bool
isForward selection = selection.direction == Forward

isBackward :: Selection -> Bool
isBackward selection = selection.direction == Backward

insert :: SelectionRange -> Maybe Word -> Selection -> Selection
insert selectionRange targetColumn selection =
  selection
    { secondaries = insert' selectionRange targetColumn selection.secondaries
    }

insert'
  :: SelectionRange
  -> Maybe Word
  -> IntervalMap SelectionRange (NonEmpty (Maybe Word))
  -> IntervalMap SelectionRange (NonEmpty (Maybe Word))
insert' selectionRange targetColumn =
  IntervalMap.insertWith (<>) selectionRange (one targetColumn)

forgetTargetColumns :: Selection -> Selection
forgetTargetColumns selection =
  selection
    { primary = (fst selection.primary, Nothing)
    , secondaries =
        IntervalMap.map (NonEmpty.map \_ -> Nothing) selection.secondaries
    }

-- TODO: Clear target columns after flipping non-reduced selection ranges
flip :: Selection -> Selection
flip selection =
  case selection.direction of
    Forward -> flipBackward selection
    Backward -> flipForward selection

flipForward :: Selection -> Selection
flipForward selection = selection{ direction = Forward }

flipBackward :: Selection -> Selection
flipBackward selection = selection{ direction = Backward }

rotateForward :: Selection -> Selection
rotateForward = do
  undefined
  -- let (oldSelectionRange, oldTargetColumn) = selection.primary
  -- case IntervalMap.lookupGT oldSelectionRange selection.secondaries of
  --   Just (newSelectionRange, targetColumns) -> do
  --     case NonEmpty.uncons targetColumns of
  --       (newTargetColumn, Nothing) ->
  --         selection
  --           { primary = (newSelectionRange, newTargetColumn)
  --           , secondaries =
  --               selection.secondaries
  --               & IntervalMap.delete newSelectionRange
  --               & insert' oldSelectionRange oldTargetColumn
  --           }
  --       (newTargetColumn, Just rest) ->
  --         selection
  --           { primary = undefined
  --           , secondaries = undefined
  --           }
  --   Nothing ->
  --     case IntervalMap.lookupMin selection.secondaries of
  --       Just (newSelectionRange, targetColumns) -> do
  --         undefined
  --         -- selection
  --         --   { primary = undefined
  --         --   , secondaries = undefined
  --         --   }
  --       Nothing ->
  --         selection

rotateBackward :: Selection -> Selection
rotateBackward = undefined

toRanges :: Selection -> NonEmpty Range
toRanges selection = do
  (selectionRange, targetColumn) <- ranges selection
  pure $ selectionRangeToRange selection.direction selectionRange targetColumn

rangeToSelectionRange :: Range -> SelectionRange
rangeToSelectionRange = uncurry SelectionRange.fromPositions . Range.toPositions

selectionRangeToRange :: Direction -> SelectionRange -> Maybe Word -> Range
selectionRangeToRange direction selectionRange targetColumn = do
  let (start, end) = SelectionRange.toPositions selectionRange

  let (anchor, cursor) =
        case direction of
          Forward -> (start, end)
          Backward -> (end, start)

  Range.fromPositions anchor cursor
  & Range.setTargetColumn targetColumn
  & fromMaybe (error "SelectionRange has an invalid target column")

-- TODO: Check that target columns are valid
isValid :: Selection -> Bool
isValid = undefined
