{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Core.SelectionRange
  ( SelectionRange

    -- * Create
  , fromPosition
  , fromPositions

    -- * Query
  , start
  , end
  , isReduced
  , isOverlapping

    -- * Modify
  , merge

    -- * Consume
  , toPositions

    -- * Internal
  , isValid
  , isStartBeforeEnd
  )
where

import Data.Default.Class (Default (..))
import Data.IntervalMap.Generic.Interval (Interval (..))
import Indigo.Core.Position (Position (..))
import Prelude hiding (flip)

data SelectionRange = SelectionRange
  { start :: !Position
  , end :: !Position
  }
  deriving stock (Show, Eq, Ord)

instance Default SelectionRange where
  def :: SelectionRange
  def = fromPosition def

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
    }

fromPositions :: Position -> Position -> SelectionRange
fromPositions position1 position2 =
  SelectionRange
    { start = min position1 position2
    , end = max position1 position2
    }

start :: SelectionRange -> Position
start selectionRange = selectionRange.start

end :: SelectionRange -> Position
end selectionRange = selectionRange.end

isReduced :: SelectionRange -> Bool
isReduced selectionRange = selectionRange.start == selectionRange.end

isOverlapping :: SelectionRange -> SelectionRange -> Bool
isOverlapping = overlaps

merge :: SelectionRange -> SelectionRange -> SelectionRange
merge selectionRange1 selectionRange2 =
  SelectionRange
    { start = min selectionRange1.start selectionRange2.start
    , end = max selectionRange1.end selectionRange2.end
    }

toPositions :: SelectionRange -> (Position, Position)
toPositions selectionRange =
  ( selectionRange.start
  , selectionRange.end
  )

isValid :: SelectionRange -> Bool
isValid selectionRange = isStartBeforeEnd selectionRange

isStartBeforeEnd :: SelectionRange -> Bool
isStartBeforeEnd selectionRange = selectionRange.start <= selectionRange.end
