{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Core.Range
  ( Range

    -- * Create
  , fromPosition
  , fromPositions

    -- * Query
  , anchor
  , cursor
  , targetColumn
  , isForward
  , isBackward
  , isReduced
  , isOverlapping

    -- * Modify
  , setTargetColumn
  , mapTargetColumn
  , forgetTargetColumn
  , flip
  , flipForward
  , flipBackward
  , reduce
  , merge

    -- * Consume
  , toPositions

    -- * Internal
  , isValid
  , isTargetColumnGreaterThanCursorColumn
  )
where

import Data.Default.Class (Default (..))
import Indigo.Core.Position (Position (..))
import Prelude hiding (flip)

data Range = Range
  { anchor :: !Position
  , cursor :: !Position
  , targetColumn :: !(Maybe Word)
  }
  deriving stock (Show, Eq)

instance Default Range where
  def :: Range
  def = fromPosition def

fromPosition :: Position -> Range
fromPosition position = fromPositions position position

fromPositions :: Position -> Position -> Range
fromPositions anchor cursor =
  Range
    { anchor
    , cursor
    , targetColumn = Nothing
    }

anchor :: Range -> Position
anchor range = range.anchor

cursor :: Range -> Position
cursor range = range.cursor

targetColumn :: Range -> Maybe Word
targetColumn range = range.targetColumn

isForward :: Range -> Bool
isForward range = range.anchor <= range.cursor

isBackward :: Range -> Bool
isBackward range = range.anchor > range.cursor

isReduced :: Range -> Bool
isReduced range = range.anchor == range.cursor

isOverlapping :: Range -> Range -> Bool
isOverlapping (flipForward -> range1) (flipForward -> range2) =
     (range1.anchor <= range2.anchor && range2.anchor <= range1.cursor)
  || (range2.anchor <= range1.anchor && range1.anchor <= range2.cursor)

setTargetColumn :: Maybe Word -> Range -> Maybe Range
setTargetColumn targetColumn = mapTargetColumn (\_ -> targetColumn)

mapTargetColumn :: (Maybe Word -> Maybe Word) -> Range -> Maybe Range
mapTargetColumn f range =
  guarded isValid range{ targetColumn = f range.targetColumn }

forgetTargetColumn :: Range -> Range
forgetTargetColumn range = range{ targetColumn = Nothing }

flip :: Range -> Range
flip range =
  if isReduced range then
    range
  else
    Range
      { anchor = range.cursor
      , cursor = range.anchor
      , targetColumn = Nothing
      }

flipForward :: Range -> Range
flipForward range =
  if isBackward range
    then flip range
    else range

flipBackward :: Range -> Range
flipBackward range =
  if isForward range
    then flip range
    else range

reduce :: Range -> Range
reduce range = range{ anchor = range.cursor }

merge :: Range -> Range -> Range
merge range1 range2 =
  case (isForward range1, isForward range2) of
    (True, True) ->
      -- Forward
      Range{ anchor, cursor, targetColumn }
      where
      anchor = min range1.anchor range2.anchor
      (cursor, targetColumn) =
        if range1.cursor > range2.cursor
          then (range1.cursor, range1.targetColumn)
          else (range2.cursor, range2.targetColumn)
    (False, False) ->
      -- Backward
      Range{ anchor, cursor, targetColumn }
      where
      anchor = max range1.anchor range2.anchor
      (cursor, targetColumn) =
        if range1.cursor < range2.cursor
          then (range1.cursor, range1.targetColumn)
          else (range2.cursor, range2.targetColumn)
    _ ->
      -- Mixed
      merge range1 (flip range2)

toPositions :: Range -> (Position, Position)
toPositions range = (range.anchor, range.cursor)

toRawParts :: Range -> (Position, Position, Maybe Word)
toRawParts range = (range.anchor, range.cursor, range.targetColumn)

isValid :: Range -> Bool
isValid range = isTargetColumnGreaterThanCursorColumn range

isTargetColumnGreaterThanCursorColumn :: Range -> Bool
isTargetColumnGreaterThanCursorColumn range =
  maybe True (range.cursor.column <) range.targetColumn
