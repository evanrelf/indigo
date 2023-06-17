{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Indigo.Range.Internal where

import GHC.Records (HasField (..))
import Indigo.Position (Position (..))
import Prelude hiding (flip, head)

data Range = UnsafeRange
  { unsafeAnchor :: Position
  , unsafeHead :: Position
  , unsafeTargetColumn :: Maybe Word
  }

pattern Range :: Position -> Position -> Maybe Word -> Range
pattern Range{ anchor, head, targetColumn } <-
  UnsafeRange
    { unsafeAnchor = anchor
    , unsafeHead = head
    , unsafeTargetColumn = targetColumn
    }

{-# COMPLETE Range #-}

instance HasField "anchor" Range Position where
  getField :: Range -> Position
  getField r = r.unsafeAnchor

instance HasField "head" Range Position where
  getField :: Range -> Position
  getField r = r.unsafeHead

instance HasField "targetColumn" Range (Maybe Word) where
  getField :: Range -> Maybe Word
  getField r = r.unsafeTargetColumn

fromPosition :: Position -> Range
fromPosition p =
  UnsafeRange
    { unsafeAnchor = p
    , unsafeHead = p
    , unsafeTargetColumn = Nothing
    }

fromPositions :: Position -> Position -> Range
fromPositions anchor head =
  UnsafeRange
    { unsafeAnchor = anchor
    , unsafeHead = head
    , unsafeTargetColumn = Nothing
    }

mkRange :: Position -> Position -> Maybe Word -> Maybe Range
mkRange anchor head targetColumn =
  if isValid range
    then Just range
    else Nothing
  where
  range =
    UnsafeRange
      { unsafeAnchor = anchor
      , unsafeHead = head
      , unsafeTargetColumn = targetColumn
      }

isValid :: Range -> Bool
isValid r =
  -- Target column must be greater than head's column
  maybe True (> r.head.column) r.targetColumn

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
  UnsafeRange
    { unsafeAnchor = r.head
    , unsafeHead = r.anchor
    , unsafeTargetColumn = Nothing
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
reduce r = r{ unsafeAnchor = r.head }

merge :: Range -> Range -> Range
merge l r =
  case (isForwards l, isForwards r) of
    (True, True) ->
      -- Forwards
      UnsafeRange
        { unsafeAnchor = min l.anchor r.anchor
        , unsafeHead = max l.head r.head
        , unsafeTargetColumn =
            if r.head > l.head
              then r.targetColumn
              else l.targetColumn
        }
    (False, False) ->
      -- Backwards
      UnsafeRange
        { unsafeAnchor = max l.anchor r.anchor
        , unsafeHead = min l.head r.head
        , unsafeTargetColumn =
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
