{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Selection
  ( Selection

    -- * Create
  , fromSelectionRange
  , fromSelectionRanges
  , mkSelection

    -- * Query
  , primary
  , above
  , below

    -- * Modify
  , insert
  , forward
  , backward

    -- * Consume
  , toSelectionRanges
  )
where

import Data.Sequence (Seq (..), (<|), (|>))
import Data.Default.Class (Default (..))
import Indigo.Range (Range)
import Indigo.SelectionRange (SelectionRange)

import qualified Indigo.SelectionRange as SelectionRange

data Selection = Selection
  { above :: Seq SelectionRange
  , primary :: SelectionRange
  , below :: Seq SelectionRange
  }

instance Default Selection where
  def :: Selection
  def = Selection{ above = mempty, primary = def, below = mempty }

fromSelectionRange :: SelectionRange -> Selection
fromSelectionRange primary =
  Selection{ above = mempty, primary, below = mempty }

fromSelectionRanges :: NonEmpty SelectionRange -> Selection
fromSelectionRanges = undefined

mkSelection
  :: Seq SelectionRange
  -> SelectionRange
  -> Seq SelectionRange
  -> Maybe Selection
mkSelection above primary below =
  if isValid selection
    then Just selection
    else Nothing
  where
  selection = Selection{ above, primary, below }

-- Must have at least one selection range
-- Primary selection range index must be valid
-- Selection ranges must be sorted
-- Selection ranges must not overlap
-- Selection ranges must face the same direction
isValid :: Selection -> Bool
isValid = undefined

primary :: Selection -> SelectionRange
primary selection = selection.primary

above :: Selection -> Seq SelectionRange
above selection = selection.above

below :: Selection -> Seq SelectionRange
below selection = selection.below

insert :: Range -> Selection -> Selection
insert range selection =
  case compare range (SelectionRange.range selection.primary) of
    LT -> undefined
    EQ -> selection
    GT -> undefined

forward :: Selection -> Selection
forward selection =
  case (selection.above, selection.below) of
    -- Only primary range, no rotation
    (Empty, Empty) ->
      selection

    -- Primary range in middle, rotate forward
    (_, primary :<| below) ->
      Selection
        { above = selection.above |> selection.primary
        , primary
        , below
        }

    -- Primary range at end, loop back to beginning
    (primary :<| below, Empty) ->
      Selection
        { above = Empty
        , primary
        , below
        }

backward :: Selection -> Selection
backward selection =
  case (selection.above, selection.below) of
    -- Only primary range, no rotation
    (Empty, Empty) ->
      selection

    -- Primary range in middle, rotate backward
    (above :|> primary, _) ->
      Selection
        { above
        , primary
        , below = selection.primary <| selection.below
        }

    -- Primary range at beginning, loop back to end
    (Empty, above :|> primary) ->
      Selection
        { above
        , primary
        , below = Empty
        }

toSelectionRanges :: Selection -> NonEmpty SelectionRange
toSelectionRanges selection =
  fromList $ toList $
    selection.above <> one selection.primary <> selection.below
