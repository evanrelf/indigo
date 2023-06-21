{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Selection
  ( Selection

    -- * Create
  , fromRange
  , fromRanges

    -- * Query
  , primary
  , above
  , below

    -- * Modify
  , rotateForward
  , rotateBackward

    -- * Consume
  , toRanges
  )
where

import Data.Default.Class (Default (..))
import Data.Sequence (Seq (..), (<|), (|>))
import Indigo.Range (Range)

data Selection = Selection
  { above :: Seq Range
  , primary :: Range
  , below :: Seq Range
  }

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
fromRanges = undefined

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

-- Ranges must be sorted
-- Ranges must not overlap
-- Ranges must face the same direction
isValid :: Selection -> Bool
isValid = undefined
