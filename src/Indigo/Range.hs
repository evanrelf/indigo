module Indigo.Range
  ( Range (Range, anchor, head, targetColumn)
  , fromPosition
  , fromPositions
  , mkRange
  , isForwards
  , isBackwards
  , isReduced
  , isOverlapping
  , flip
  , flipForwards
  , flipBackwards
  , reduce
  , merge
  )
where

import Indigo.Range.Internal
import Prelude hiding (flip, head)
