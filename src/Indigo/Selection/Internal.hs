{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Selection.Internal where

import Indigo.Range (Range)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Indigo.Range as Range

data Selection = Selection
  { above :: Seq Range
  , primary :: Range
  , below :: Seq Range
  }

fromRange :: Range -> Selection
fromRange r =
  Selection
    { above = mempty
    , primary = r
    , below = mempty
    }

fromRanges :: NonEmpty Range -> Selection
fromRanges ranges =
  case NonEmpty.uncons ranges of
    (r, Nothing) -> fromRange r
    (r, Just rs) -> insert r (fromRanges rs)

mkSelection :: Seq Range -> Range -> Seq Range -> Maybe Selection
mkSelection above primary below =
  if isValid selection
    then Just selection
    else Nothing
  where
  selection = Selection{ above, primary, below }

isValid :: Selection -> Bool
isValid s =
  and
    [ -- Must have at least one range
      undefined
      -- Primary range index must be valid
    , undefined
      -- Ranges must be sorted
    , undefined
      -- Ranges must not overlap
    , undefined
    ]

above :: Selection -> Seq Range
above s = s.above

primary :: Selection -> Range
primary s = s.primary

below :: Selection -> Seq Range
below s = s.below

insert :: Range -> Selection -> Selection
insert r s =
  case comparing Range.toPositions r s.primary of
    LT -> undefined
    EQ -> s
    GT -> undefined

toRanges :: Selection -> NonEmpty Range
toRanges s = undefined $ s.above <> one s.primary <> s.below
