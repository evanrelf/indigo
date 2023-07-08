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
  , isForward
  , isBackward

    -- * Modify
  , flip
  , flipForward
  , flipBackward
  , rotateForward
  , rotateBackward

    -- * Consume
  )
where

import Data.Default.Class (Default (..))
import Data.IntervalMap.Generic.Strict (IntervalMap)
import Indigo.Core.Direction (Direction (..))
import Indigo.Core.Range (Range)
import Indigo.Core.SelectionRange (SelectionRange)
import Prelude hiding (flip)

import qualified Data.IntervalMap.Generic.Strict as IntervalMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Indigo.Core.Range as Range
import qualified Indigo.Core.SelectionRange as SelectionRange

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
rotateForward selection = do
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
rotateBackward selection = undefined

rangeToSelectionRange :: Range -> SelectionRange
rangeToSelectionRange = uncurry SelectionRange.fromPositions . Range.toPositions
