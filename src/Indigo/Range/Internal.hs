{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Range.Internal where

import Indigo.Position (Position (..))
import Prelude hiding (flip, head)

data Range = Range
  { anchor :: Position
  , head :: Position
  , targetColumn :: Maybe Word
  }

fromPosition :: Position -> Range
fromPosition p =
  Range
    { anchor = p
    , head = p
    , targetColumn = Nothing
    }

fromPositions :: Position -> Position -> Range
fromPositions anchor head = Range{ anchor, head, targetColumn = Nothing }

mkRange :: Position -> Position -> Maybe Word -> Maybe Range
mkRange anchor head targetColumn =
  if isValid range
    then Just range
    else Nothing
  where
  range = Range{ anchor, head, targetColumn }

isValid :: Range -> Bool
isValid r =
  -- Target column must be greater than head's column
  maybe True (> r.head.column) r.targetColumn

anchor :: Range -> Position
anchor r = r.anchor

head :: Range -> Position
head r = r.head

targetColumn :: Range -> Maybe Word
targetColumn r = r.targetColumn

isForwards :: Range -> Bool
isForwards r = r.anchor <= r.head

isBackwards :: Range -> Bool
isBackwards r = r.anchor > r.head

isReduced :: Range -> Bool
isReduced r = r.anchor == r.head

isOverlapping :: Range -> Range -> Bool
isOverlapping (flipForwards -> l) (flipForwards -> r) =
     (l.anchor <= r.anchor && r.anchor <= l.head)
  || (r.anchor <= l.anchor && l.anchor <= r.head)

flip :: Range -> Range
flip r =
  Range
    { anchor = r.head
    , head = r.anchor
    , targetColumn = Nothing
    }

flipForwards :: Range -> Range
flipForwards r =
  if isBackwards r
    then flip r
    else r

flipBackwards :: Range -> Range
flipBackwards r =
  if isForwards r
    then flip r
    else r

reduce :: Range -> Range
reduce r = r{ anchor = r.head }

merge :: Range -> Range -> Range
merge l r =
  case (isForwards l, isForwards r) of
    (True, True) ->
      -- Forwards
      Range
        { anchor = min l.anchor r.anchor
        , head = max l.head r.head
        , targetColumn =
            if r.head > l.head
              then r.targetColumn
              else l.targetColumn
        }
    (False, False) ->
      -- Backwards
      Range
        { anchor = max l.anchor r.anchor
        , head = min l.head r.head
        , targetColumn =
            if r.head < l.head
              then r.targetColumn
              else l.targetColumn
        }
    _ ->
      -- Mixed
      merge l (flip r)

instance Semigroup Range where
  (<>) :: Range -> Range -> Range
  (<>) = merge

forgetTargetColumn :: Range -> Range
forgetTargetColumn r = r{ targetColumn = Nothing }

toPositions :: Range -> (Position, Position)
toPositions r = (r.anchor, r.head)
