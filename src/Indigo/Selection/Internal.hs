{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Selection.Internal where

import Indigo.Range (Range)

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
fromRanges = undefined

mkSelection :: Seq Range -> Range -> Seq Range -> Maybe Selection
mkSelection above primary below =
  if isValid selection
    then Just selection
    else Nothing
  where
  selection = Selection{ above, primary, below }

-- Must have at least one range
-- Primary range index must be valid
-- Ranges must be sorted
-- Ranges must not overlap
-- Ranges must face the same direction
isValid :: Selection -> Bool
isValid = undefined

primary :: Selection -> Range
primary s = s.primary

above :: Selection -> Seq Range
above s = s.above

below :: Selection -> Seq Range
below s = s.below

insert :: Range -> Selection -> Selection
insert (Range.forgetTargetColumn -> r) s =
  case comparing Range.toPositions r s.primary of
    LT -> undefined
    EQ -> s
    GT -> undefined

toRanges :: Selection -> NonEmpty Range
toRanges s = fromList $ toList $ s.above <> one s.primary <> s.below
