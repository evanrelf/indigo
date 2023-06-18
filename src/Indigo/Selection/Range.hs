{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Selection.Range
  ( SelectionRange

    -- * Create
  , fromPosition
  , fromPositions
  , fromRange
  , mkSelectionRange

    -- * Query
  , anchor
  , head
  , range
  , targetColumn
  , isForwards
  , isBackwards
  , isReduced
  , isOverlapping

    -- * Modify
  , flip
  , flipForwards
  , flipBackwards
  , reduce
  , merge
  , forgetTargetColumn
  )
where

import Data.Default.Class (Default (..))
import Indigo.Position (Position (..))
import Indigo.Range (Range)
import Prelude hiding (flip, head)

import qualified Indigo.Range as Range

data SelectionRange = SelectionRange
  { range :: Range
  , targetColumn :: Maybe Word
  }

instance Default SelectionRange where
  def :: SelectionRange
  def = SelectionRange{ range = def, targetColumn = Nothing }

fromPosition :: Position -> SelectionRange
fromPosition position = fromRange $ Range.fromPosition position

fromPositions :: Position -> Position -> SelectionRange
fromPositions anchor head = fromRange $ Range.fromPositions anchor head

fromRange :: Range -> SelectionRange
fromRange range = SelectionRange{ range, targetColumn = Nothing }

mkSelectionRange :: Range -> Maybe Word -> Maybe SelectionRange
mkSelectionRange range targetColumn =
  if isValid selectionRange
    then Just selectionRange
    else Nothing
  where
  selectionRange = SelectionRange{ range, targetColumn }

isValid :: SelectionRange -> Bool
isValid selectionRange =
  -- Target column must be greater than head's column
  maybe True (> selectionRange.range.head.column) selectionRange.targetColumn

anchor :: SelectionRange -> Position
anchor selectionRange = selectionRange.range.anchor

head :: SelectionRange -> Position
head selectionRange = selectionRange.range.head

range :: SelectionRange -> Range
range selectionRange = selectionRange.range

targetColumn :: SelectionRange -> Maybe Word
targetColumn selectionRange = selectionRange.targetColumn

isForwards :: SelectionRange -> Bool
isForwards selectionRange = Range.isForwards selectionRange.range

isBackwards :: SelectionRange -> Bool
isBackwards selectionRange = Range.isBackwards selectionRange.range

isReduced :: SelectionRange -> Bool
isReduced selectionRange = Range.isReduced selectionRange.range

isOverlapping :: SelectionRange -> SelectionRange -> Bool
isOverlapping left right = Range.isOverlapping left.range right.range

flip :: SelectionRange -> SelectionRange
flip selectionRange =
  selectionRange{ range = Range.flip selectionRange.range }

flipForwards :: SelectionRange -> SelectionRange
flipForwards selectionRange =
  if isBackwards selectionRange
    then flip selectionRange
    else selectionRange

flipBackwards :: SelectionRange -> SelectionRange
flipBackwards selectionRange =
  if isForwards selectionRange
    then flip selectionRange
    else selectionRange

reduce :: SelectionRange -> SelectionRange
reduce selectionRange =
  selectionRange{ range = Range.reduce selectionRange.range }

merge :: SelectionRange -> SelectionRange -> SelectionRange
merge left right =
  SelectionRange
    { range = Range.merge left.range right.range
    , targetColumn = undefined
    }

instance Semigroup SelectionRange where
  (<>) :: SelectionRange -> SelectionRange -> SelectionRange
  (<>) = merge

forgetTargetColumn :: SelectionRange -> SelectionRange
forgetTargetColumn selectionRange = selectionRange{ targetColumn = Nothing }
