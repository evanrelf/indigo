{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Indigo.Selection.Internal where

import GHC.Records (HasField (..))
import Indigo.Range (Range)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Indigo.Range as Range

data Selection = UnsafeSelection
  { unsafeAbove :: Seq Range
  , unsafePrimary :: Range
  , unsafeBelow :: Seq Range
  }

pattern Selection :: Seq Range -> Range -> Seq Range -> Selection
pattern Selection{ above, primary, below } <-
  UnsafeSelection
    { unsafeAbove = above
    , unsafePrimary = primary
    , unsafeBelow = below
    }

{-# COMPLETE Selection #-}

instance HasField "above" Selection (Seq Range) where
  getField :: Selection -> Seq Range
  getField s = s.unsafeAbove

instance HasField "primary" Selection Range where
  getField :: Selection -> Range
  getField s = s.unsafePrimary

instance HasField "below" Selection (Seq Range) where
  getField :: Selection -> Seq Range
  getField s = s.unsafeBelow

fromRange :: Range -> Selection
fromRange r =
  UnsafeSelection
    { unsafeAbove = mempty
    , unsafePrimary = r
    , unsafeBelow = mempty
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

insert :: Range -> Selection -> Selection
insert r s =
  case comparing Range.toPositions r s.primary of
    LT -> undefined
    EQ -> s
    GT -> undefined

toRanges :: Selection -> NonEmpty Range
toRanges s = undefined $ s.above <> one s.primary <> s.below
