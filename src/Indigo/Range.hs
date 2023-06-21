{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Range
  ( Range

    -- * Create
  , fromPosition
  , fromPositions
  , fromRawParts
  , unsafeFromRawParts

    -- * Query
  , anchor
  , cursor
  , targetColumn
  , isForward
  , isBackward
  , isReduced
  , isOverlapping

    -- * Modify
  , forgetTargetColumn
  , flip
  , flipForward
  , flipBackward
  , reduce
  , merge

    -- * Consume
  , toPositions
  , toRawParts
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
  deriving stock (Eq)

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

fromRawParts :: Position -> Position -> Maybe Word -> Maybe Range
fromRawParts anchor cursor targetColumn =
  if isValid r
    then Just r
    else Nothing
  where
  r = Range{ anchor, cursor, targetColumn }

-- $
-- Target column must be greater than cursor column
-- >>> isJust $ fromRawParts (Position 0 0) (Position 0 20) (Just 21)
-- True
-- >>> isJust $ fromRawParts (Position 0 0) (Position 0 20) (Just 20)
-- False

unsafeFromRawParts :: Position -> Position -> Maybe Word -> Range
unsafeFromRawParts anchor cursor targetColumn =
  Range{ anchor, cursor, targetColumn }

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

-- $
-- >>> let r1 = fromPositions (Position 0 0) (Position 1 0)
-- >>> let r2 = fromPositions (Position 1 0) (Position 2 0)
-- >>> isOverlapping r1 r2 && isOverlapping r2 r1
-- True
-- >>> let r1 = fromPositions (Position 0 0) (Position 2 0)
-- >>> let r2 = fromPositions (Position 1 0) (Position 3 0)
-- >>> isOverlapping r1 r2 && isOverlapping r2 r1
-- True
-- >>> let r1 = fromPositions (Position 0 0) (Position 0 1)
-- >>> let r2 = fromPositions (Position 0 2) (Position 0 3)
-- >>> isOverlapping r1 r2 && isOverlapping r2 r1
-- False

forgetTargetColumn :: Range -> Range
forgetTargetColumn r = r{ targetColumn = Nothing }

flip :: Range -> Range
flip r =
  Range
    { anchor = r.cursor
    , cursor = r.anchor
    , targetColumn = Nothing
    }

-- $
-- Flipping should forget the target column
-- >>> let Just r = fromRawParts (Position 0 0) (Position 1 0) (Just 42)
-- >>> forgetTargetColumn r == flip (flip r)
-- True

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

-- $
-- Reducing should not forget the target column
-- >>> let Just r = fromRawParts (Position 0 0) (Position 0 0) (Just 42)
-- >>> r == reduce r
-- True

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

-- $
-- >>> let Just r1 = fromRawParts (Position 0 0) (Position 1 0) (Just 21)
-- >>> let Just r2 = fromRawParts (Position 3 0) (Position 4 0) (Just 42)
-- >>> merge r1 r2 == merge r2 r1
-- True
-- >>> targetColumn (merge r1 r2) == Just 42
-- True

toPositions :: Range -> (Position, Position)
toPositions r = (r.anchor, r.cursor)

toRawParts :: Range -> (Position, Position, Maybe Word)
toRawParts r = (r.anchor, r.cursor, r.targetColumn)

isValid :: Range -> Bool
isValid r =
  -- Target column must be greater than cursor column
  maybe True (r.cursor.column <) r.targetColumn

-- $setup
-- >>> :m -Prelude
-- >>> import Relude hiding (flip)
-- >>> import Indigo.Position (Position (..))
