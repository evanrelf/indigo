{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Core.SelectionRange
  ( SelectionRange

    -- * Create
  , fromPosition
  , fromPositions
  , fromRawParts

    -- * Query
  , start
  , end
  , targetColumn
  , isReduced
  , isOverlapping

    -- * Modify
  , mapTargetColumn
  , merge

    -- * Consume
  , toPositions
  , toRawParts

    -- * Internal
  , unsafeFromRawParts
  , isValid
  , isStartBeforeEnd
  )
where

import Data.Default.Class (Default (..))
import Data.IntervalMap.Generic.Interval (Interval (..))
import Indigo.Core.Position (Position (..))
import Prelude hiding (flip)

data SelectionRange = SelectionRange
  { start :: Position
  , end :: Position
    -- TODO: Maybe switch to `IntervalMap SelectionRange (Maybe Word)` so that
    -- the `Eq` and `Ord` instances aren't gross (can be derived), and then the
    -- selection can handle choosing the right target column.
  , targetColumn :: Maybe Word
  }
  deriving stock (Show)

instance Default SelectionRange where
  def :: SelectionRange
  def = fromPosition def

instance Eq SelectionRange where
  (==) = (==) `on` toPositions

instance Ord SelectionRange where
  compare :: SelectionRange -> SelectionRange -> Ordering
  compare = compare `on` toPositions

instance Interval SelectionRange Position where
  lowerBound :: SelectionRange -> Position
  lowerBound selectionRange = selectionRange.start

  upperBound :: SelectionRange -> Position
  upperBound selectionRange = selectionRange.end

fromPosition :: Position -> SelectionRange
fromPosition position =
  SelectionRange
    { start = position
    , end = position
    , targetColumn = Nothing
    }

fromPositions :: Position -> Position -> SelectionRange
fromPositions position1 position2 =
  SelectionRange
    { start = min position1 position2
    , end = max position1 position2
    , targetColumn = Nothing
    }

fromRawParts :: Position -> Position -> Maybe Word -> Maybe SelectionRange
fromRawParts start end targetColumn =
  if isValid r
    then Just r
    else Nothing
  where
  r = SelectionRange{ start, end, targetColumn }

unsafeFromRawParts :: Position -> Position -> Maybe Word -> SelectionRange
unsafeFromRawParts start end targetColumn =
  SelectionRange{ start, end, targetColumn }

start :: SelectionRange -> Position
start selectionRange = selectionRange.start

end :: SelectionRange -> Position
end selectionRange = selectionRange.end

targetColumn :: SelectionRange -> Maybe Word
targetColumn selectionRange = selectionRange.targetColumn

isReduced :: SelectionRange -> Bool
isReduced selectionRange = selectionRange.start == selectionRange.end

isOverlapping :: SelectionRange -> SelectionRange -> Bool
isOverlapping = overlaps

mapTargetColumn :: (Maybe Word -> Maybe Word) -> SelectionRange -> SelectionRange
mapTargetColumn f selectionRange =
  selectionRange{ targetColumn = f selectionRange.targetColumn }

merge :: SelectionRange -> SelectionRange -> SelectionRange
merge selectionRange1 selectionRange2 =
  SelectionRange
    { start = min selectionRange1.start selectionRange2.start
    , end = max selectionRange1.end selectionRange2.end
    , targetColumn =
        if selectionRange1.end > selectionRange2.end
          then selectionRange1.targetColumn
          else selectionRange2.targetColumn
    }

toPositions :: SelectionRange -> (Position, Position)
toPositions selectionRange =
  ( selectionRange.start
  , selectionRange.end
  )

toRawParts :: SelectionRange -> (Position, Position, Maybe Word)
toRawParts selectionRange =
  ( selectionRange.start
  , selectionRange.end
  , selectionRange.targetColumn
  )

isValid :: SelectionRange -> Bool
isValid selectionRange = isStartBeforeEnd selectionRange

isStartBeforeEnd :: SelectionRange -> Bool
isStartBeforeEnd selectionRange = selectionRange.start <= selectionRange.end
