{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Range
  ( Range

    -- * Create
  , fromPosition
  , fromPositions
  , fromPositionsAndTarget

    -- * Query
  , anchor
  , cursor
  , targetColumn
  , isForward
  , isBackward
  , isReduced
  , isOverlapping

    -- * Modify
  , flip
  , flipForward
  , flipBackward
  , reduce
  , forgetTargetColumn

    -- * Consume
  , toPositions
  )
where

import Data.Default.Class (Default (..))
import Indigo.Position (Position (..))
import Prelude hiding (flip)

data Range = Range
  { anchor :: Position
  , cursor :: Position
  , targetColumn :: Maybe Word
  }

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

fromPositionsAndTarget :: Position -> Position -> Word -> Maybe Range
fromPositionsAndTarget anchor cursor (Just -> targetColumn) =
  if isValid r
    then Just r
    else Nothing
  where
  r = Range{ anchor, cursor, targetColumn }

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

forgetTargetColumn :: Range -> Range
forgetTargetColumn r = r{ targetColumn = Nothing }

toPositions :: Range -> (Position, Position)
toPositions r = (r.anchor, r.cursor)

isValid :: Range -> Bool
isValid r =
  -- Target column must be greater than cursor column
  maybe True (r.cursor.column <) r.targetColumn
