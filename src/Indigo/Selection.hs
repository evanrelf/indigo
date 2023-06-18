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

import Prelude hiding (lefts, rights)
import Data.Default.Class (Default (..))
import Indigo.Range (Range)
import Indigo.Selection.Range (SelectionRange)
import Indigo.Selection.Zipper (SelectionZipper (..))

import qualified Indigo.Selection.Range as SelectionRange
import qualified Indigo.Selection.Zipper as SelectionZipper

newtype Selection = Selection (SelectionZipper SelectionRange)

instance Default Selection where
  def :: Selection
  def = Selection SelectionZipper
    { lefts = mempty
    , focus = def
    , rights = mempty
    }

fromSelectionRange :: SelectionRange -> Selection
fromSelectionRange focus =
  Selection SelectionZipper
    { lefts = mempty
    , focus
    , rights = mempty
    }

fromSelectionRanges :: NonEmpty SelectionRange -> Selection
fromSelectionRanges = undefined

mkSelection
  :: Seq SelectionRange
  -> SelectionRange
  -> Seq SelectionRange
  -> Maybe Selection
mkSelection lefts focus rights =
  if isValid selection
    then Just selection
    else Nothing
  where
  selection = Selection SelectionZipper{ lefts, focus, rights }

-- Must have at least one selection range
-- Primary selection range index must be valid
-- Selection ranges must be sorted
-- Selection ranges must not overlap
-- Selection ranges must face the same direction
isValid :: Selection -> Bool
isValid = undefined

primary :: Selection -> SelectionRange
primary (Selection zipper) = zipper.focus

above :: Selection -> Seq SelectionRange
above (Selection zipper) = zipper.lefts

below :: Selection -> Seq SelectionRange
below (Selection zipper) = zipper.rights

insert :: Range -> Selection -> Selection
insert range selection@(Selection zipper) =
  case compare range (SelectionRange.range zipper.focus) of
    LT -> undefined
    EQ -> selection
    GT -> undefined

forward :: Selection -> Selection
forward (Selection zipper) = Selection (SelectionZipper.right zipper)

backward :: Selection -> Selection
backward (Selection zipper) = Selection (SelectionZipper.left zipper)

toSelectionRanges :: Selection -> NonEmpty SelectionRange
toSelectionRanges (Selection zipper) =
  fromList $ toList $
    zipper.lefts <> one zipper.focus <> zipper.rights
