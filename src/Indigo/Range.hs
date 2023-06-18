{-# LANGUAGE ViewPatterns #-}

module Indigo.Range
  ( Range (..)

    -- * Create
  , fromPosition
  , fromPositions

    -- * Query
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
  )
where

import Data.Default.Class (Default (..))
import Indigo.Position (Position)
import Prelude hiding (flip, head)

data Range = Range
  { anchor :: Position
  , head :: Position
  }
  deriving stock (Eq, Ord)

instance Default Range where
  def :: Range
  def = Range{ anchor = def, head = def }

fromPosition :: Position -> Range
fromPosition position = Range{ anchor = position, head = position }

fromPositions :: Position -> Position -> Range
fromPositions anchor head = Range{ anchor, head }

isForwards :: Range -> Bool
isForwards range = range.anchor <= range.head

isBackwards :: Range -> Bool
isBackwards range = range.anchor > range.head

isReduced :: Range -> Bool
isReduced range = range.anchor == range.head

isOverlapping :: Range -> Range -> Bool
isOverlapping (flipForwards -> left) (flipForwards -> right) =
     (left.anchor <= right.anchor && right.anchor <= left.head)
  || (right.anchor <= left.anchor && left.anchor <= right.head)

flip :: Range -> Range
flip range = Range{ anchor = range.head, head = range.anchor }

flipForwards :: Range -> Range
flipForwards range =
  if isBackwards range
    then flip range
    else range

flipBackwards :: Range -> Range
flipBackwards range =
  if isForwards range
    then flip range
    else range

reduce :: Range -> Range
reduce range = range{ anchor = range.head }

merge :: Range -> Range -> Range
merge left right =
  case (isForwards left, isForwards right) of
    (True, True) ->
      -- Forwards
      Range
        { anchor = min left.anchor right.anchor
        , head = max left.head right.head
        }
    (False, False) ->
      -- Backwards
      Range
        { anchor = max left.anchor right.anchor
        , head = min left.head right.head
        }
    _ ->
      -- Mixed
      merge left (flip right)

instance Semigroup Range where
  (<>) :: Range -> Range -> Range
  (<>) = merge
