{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Core.Selection2
  ( Selection

    -- * Create

    -- * Query
  , primary
  , isForward
  , isBackward

    -- * Modify
  , flip
  , flipForward
  , flipBackward
  , rotateForward
  , rotateBackward

    -- * Consume
  )
where

import Data.IntervalMap.Generic.Strict (IntervalMap)
import Indigo.Core.Range (Range)
import Indigo.Core.SelectionRange (SelectionRange)
import Prelude hiding (flip)

import qualified Data.IntervalMap.Generic.Strict as IntervalMap

data Selection = Selection
  { ranges :: IntervalMap SelectionRange (NonEmpty (Maybe Word))
  , primary :: (SelectionRange, Maybe Word)
  , direction :: Direction
  }
  deriving stock (Show, Eq)

data Direction
  = Forward
  | Backward
  deriving stock (Show, Eq)

primary :: Selection -> Range
primary selection = undefined selection.primary

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
