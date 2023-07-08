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
fromPosition p = fromPositions p p

fromPositions :: Position -> Position -> Range
fromPositions anchor cursor =
  Range
    { anchor
    , cursor
    , targetColumn = Nothing
    }

anchor :: Range -> Position
anchor r = r.anchor

cursor :: Range -> Position
cursor r = r.cursor

targetColumn :: Range -> Maybe Word
targetColumn r = r.targetColumn

isForward :: Range -> Bool
isForward r = r.anchor <= r.cursor

isBackward :: Range -> Bool
isBackward r = r.anchor > r.cursor

isReduced :: Range -> Bool
isReduced r = r.anchor == r.cursor

isOverlapping :: Range -> Range -> Bool
isOverlapping (flipForward -> r1) (flipForward -> r2) =
     (r1.anchor <= r2.anchor && r2.anchor <= r1.cursor)
  || (r2.anchor <= r1.anchor && r1.anchor <= r2.cursor)

mapTargetColumn :: (Maybe Word -> Maybe Word) -> Range -> Maybe Range
mapTargetColumn f range =
  guarded isValid range{ targetColumn = f range.targetColumn }

forgetTargetColumn :: Range -> Range
forgetTargetColumn r = r{ targetColumn = Nothing }

flip :: Range -> Range
flip r =
  Range
    { anchor = r.cursor
    , cursor = r.anchor
    , targetColumn = Nothing
    }

flipForward :: Range -> Range
flipForward r =
  if isBackward r
    then flip r
    else r

flipBackward :: Range -> Range
flipBackward r =
  if isForward r
    then flip r
    else r

reduce :: Range -> Range
reduce r = r{ anchor = r.cursor }

merge :: Range -> Range -> Range
merge r1 r2 =
  case (isForward r1, isForward r2) of
    (True, True) ->
      -- Forward
      Range{ anchor, cursor, targetColumn }
      where
      anchor = min r1.anchor r2.anchor
      (cursor, targetColumn) =
        if r1.cursor > r2.cursor
          then (r1.cursor, r1.targetColumn)
          else (r2.cursor, r2.targetColumn)
    (False, False) ->
      -- Backward
      Range{ anchor, cursor, targetColumn }
      where
      anchor = max r1.anchor r2.anchor
      (cursor, targetColumn) =
        if r1.cursor < r2.cursor
          then (r1.cursor, r1.targetColumn)
          else (r2.cursor, r2.targetColumn)
    _ ->
      -- Mixed
      merge r1 (flip r2)

toPositions :: Range -> (Position, Position)
toPositions r = (r.anchor, r.cursor)

toRawParts :: Range -> (Position, Position, Maybe Word)
toRawParts r = (r.anchor, r.cursor, r.targetColumn)

isValid :: Range -> Bool
isValid r = isTargetColumnGreaterThanCursorColumn r

isTargetColumnGreaterThanCursorColumn :: Range -> Bool
isTargetColumnGreaterThanCursorColumn r =
  maybe True (r.cursor.column <) r.targetColumn
