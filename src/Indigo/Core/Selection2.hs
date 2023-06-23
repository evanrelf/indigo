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

import Data.IntervalSet (IntervalSet)
import Indigo.Core.Range (Range)
import Indigo.Core.SelectionRange (SelectionRange)
import Prelude hiding (flip)

import qualified Data.IntervalSet as IntervalSet

data Selection = Selection
  { above :: IntervalSet SelectionRange
  , primary :: SelectionRange
  , below :: IntervalSet SelectionRange
  , direction :: Direction
  }
  deriving stock (Show, Eq)

data Direction
  = Forward
  | Backward
  deriving stock (Show, Eq)

-- unsafeFromRawParts
--   :: IntervalSet SelectionRange
--   -> SelectionRange
--   -> IntervalSet SelectionRange
--   -> Direction
--   -> Selection
-- unsafeFromRawParts = undefined

primary :: Selection -> Range
primary selection = undefined selection.primary

above :: Selection -> [Range]
above selection =
  fmap undefined $ IntervalSet.toAscList selection.above

below :: Selection -> [Range]
below selection =
  fmap undefined $ IntervalSet.toAscList selection.below

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
rotateForward selection =
  case (selection.above, selection.below) of
    (IntervalSet.Nil, IntervalSet.Nil) ->
      selection

    (_, IntervalSet.Node{}) -> do
      let (primary, below) = IntervalSet.deleteFindMin selection.below
      Selection
        { above = IntervalSet.insert selection.primary selection.above
        , primary
        , below
        , direction = selection.direction
        }

    (IntervalSet.Node{}, IntervalSet.Nil) -> do
      let (primary, below) = IntervalSet.deleteFindMin selection.above
      Selection
        { above = IntervalSet.empty
        , primary
        , below
        , direction = selection.direction
        }

rotateBackward :: Selection -> Selection
rotateBackward selection =
  case (selection.above, selection.below) of
    (IntervalSet.Nil, IntervalSet.Nil) ->
      selection

    (IntervalSet.Node{}, _) -> do
      let (primary, above) = IntervalSet.deleteFindMax selection.below
      Selection
        { above
        , primary
        , below = IntervalSet.insert selection.primary selection.below
        , direction = selection.direction
        }

    (IntervalSet.Nil, IntervalSet.Node{}) -> do
      let (primary, above) = IntervalSet.deleteFindMax selection.above
      Selection
        { above
        , primary
        , below = IntervalSet.empty
        , direction = selection.direction
        }

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

-- TODO: `above` and `below` should be sorted thanks to the `IntervalSet`, but
-- the entire selection still needs to be sorted
isRangesAreSorted :: Selection -> Bool
isRangesAreSorted selection =
  and
    [ maybe True (selection.primary >) (IntervalSet.findMax selection.above)
    , maybe True (selection.primary <) (IntervalSet.findMin selection.below)
    ]

isRangesNotOverlapping :: Selection -> Bool
isRangesNotOverlapping = undefined
