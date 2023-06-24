{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Core.Selection2
  ( Selection

    -- * Create

    -- * Query
  , primary
  , above
  , below
  , isForward
  , isBackward

    -- * Modify
  , flip
  , flipForward
  , flipBackward
  , rotateForward
  , rotateBackward

    -- * Consume

    -- * Internal
  , isValid
  , isRangesAreSorted
  , isRangesNotOverlapping
  )
where

import Data.IntervalMap.Generic.Strict (IntervalMap)
import Indigo.Core.Range (Range)
import Indigo.Core.SelectionRange (SelectionRange)
import Prelude hiding (flip)

import qualified Data.IntervalMap.Generic.Strict as IntervalMap

-- TODO: Actually allowing duplicate ranges is probably necessary, even if it's
-- usually not exposed to the user. The `IntervalMap` package might not work,
-- unless I can modify `{Eq,Ord} SelectionRange` instances to never equate two
-- values, and avoid breaking things.
data Selection = Selection
  { above :: IntervalMap SelectionRange (NonEmpty (Maybe Word))
  , primary :: (SelectionRange, Maybe Word)
  , below :: IntervalMap SelectionRange (NonEmpty (Maybe Word))
  , direction :: Direction
  }
  deriving stock (Show, Eq)

data Direction
  = Forward
  | Backward
  deriving stock (Show, Eq)

primary :: Selection -> Range
primary selection = undefined selection.primary

above :: Selection -> [Range]
above selection = undefined

below :: Selection -> [Range]
below selection = undefined

isForward :: Selection -> Bool
isForward selection = selection.direction == Forward

isBackward :: Selection -> Bool
isBackward selection = selection.direction == Backward

flip :: Selection -> Selection
flip selection =
  case selection.direction of
    Forward -> flipBackward selection
    Backward -> flipForward selection

flipForward :: Selection -> Selection
flipForward selection = selection{ direction = Forward }

flipBackward :: Selection -> Selection
flipBackward selection = selection{ direction = Backward }

rotateForward :: Selection -> Selection
rotateForward selection = undefined

rotateBackward :: Selection -> Selection
rotateBackward selection = undefined

-- TODO: Make sure direction and target column are good
selectionRangeToRange :: Direction -> SelectionRange -> Maybe Range
selectionRangeToRange = undefined

rangeToSelectionRange :: Range -> Maybe SelectionRange
rangeToSelectionRange = undefined

isValid :: Selection -> Bool
isValid selection =
  and
    [ isRangesAreSorted selection
    , isRangesNotOverlapping selection
    ]

-- TODO: `above` and `below` should be sorted thanks to the `IntervalMap`, but
-- the entire selection still needs to be sorted
isRangesAreSorted :: Selection -> Bool
isRangesAreSorted selection = undefined

isRangesNotOverlapping :: Selection -> Bool
isRangesNotOverlapping = undefined
