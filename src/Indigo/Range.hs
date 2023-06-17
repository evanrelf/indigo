module Indigo.Range
  ( Range
  , fromPosition
  , fromPositions
  , mkRange
  , anchor
  , head
  , targetColumn
  , isForwards
  , isBackwards
  , isReduced
  , isOverlapping
  , flip
  , flipForwards
  , flipBackwards
  , reduce
  , merge
  , forgetTargetColumn
  , toPositions
  )
where

import Indigo.Range.Internal
import Prelude hiding (flip, head)
