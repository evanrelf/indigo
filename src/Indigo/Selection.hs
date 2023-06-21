{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Selection
  ( Selection

    -- * Create
  , fromRange
  , fromRanges
  , fromRawParts
  , unsafeFromRawParts

    -- * Query
  , primary
  , above
  , below

    -- * Modify
  , rotateForward
  , rotateBackward

    -- * Consume
  , toRanges
  , toRawParts
  )
where

import Data.Default.Class (Default (..))
import Data.Foldable (foldr1)
import Data.Sequence (Seq (..), (<|), (|>))
import Indigo.Range (Range)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Indigo.Range as Range

data Selection = Selection
  { above :: Seq Range
  , primary :: Range
  , below :: Seq Range
  }
  deriving stock (Eq)

instance Default Selection where
  def :: Selection
  def = fromRange def

fromRange :: Range -> Selection
fromRange r =
  Selection
    { above = mempty
    , primary = r
    , below = mempty
    }

fromRanges :: NonEmpty Range -> Selection
fromRanges rs =
  rs
  -- Normalize ranges
  & fmap Range.flipForward
  & fmap Range.forgetTargetColumn
  -- Merge overlapping ranges
  & NonEmpty.sortWith Range.toPositions
  & NonEmpty.groupBy1 Range.isOverlapping
  & fmap (foldr1 Range.merge)
  -- Create selection
  & buildSelection
  where
  buildSelection :: NonEmpty Range -> Selection
  buildSelection =
    NonEmpty.uncons >>> \case
      (r, Nothing) -> fromRange r
      (r, Just rs) -> let s = buildSelection rs in s{ above = r :<| s.above }

fromRawParts :: Seq Range -> Range -> Seq Range -> Maybe Selection
fromRawParts above primary below =
  if isValid s
    then Just s
    else Nothing
  where
  s = Selection{ above, primary, below }

unsafeFromRawParts :: Seq Range -> Range -> Seq Range -> Selection
unsafeFromRawParts above primary below = Selection{ above, primary, below }

primary :: Selection -> Range
primary s = s.primary

above :: Selection -> Seq Range
above s = s.above

below :: Selection -> Seq Range
below s = s.below

rotateForward :: Selection -> Selection
rotateForward s =
  case (s.above, s.below) of
    (Empty, Empty) ->
      s

    (_, primary :<| below) ->
      Selection
        { above = s.above |> s.primary
        , primary
        , below
        }

    (primary :<| below, Empty) ->
      Selection
        { above = Empty
        , primary
        , below
        }

rotateBackward :: Selection -> Selection
rotateBackward s =
  case (s.above, s.below) of
    (Empty, Empty) ->
      s

    (above :|> primary, _) ->
      Selection
        { above
        , primary
        , below = s.primary <| s.below
        }

    (Empty, above :|> primary) ->
      Selection
        { above
        , primary
        , below = Empty
        }

toRanges :: Selection -> NonEmpty Range
toRanges s = fromList $ toList $ s.above <> one s.primary <> s.below

toRawParts :: Selection -> (Seq Range, Range, Seq Range)
toRawParts s = (s.above, s.primary, s.below)

isValid :: Selection -> Bool
isValid s =
  and
    [ -- Ranges must be sorted
      ranges == sortedRanges

    , -- Ranges must face the same direction
      all Range.isForward ranges || all Range.isBackward ranges

    , -- Ranges must not overlap
      all
        (length >>> (== 1))
        (NonEmpty.groupAllWith1 Range.toPositions sortedRanges)
    ]
  where
  ranges = toRanges s
  sortedRanges = NonEmpty.sortWith Range.toPositions ranges
