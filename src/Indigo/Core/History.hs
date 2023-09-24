{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | <https://github.com/zaboople/klonk/blob/master/TheGURQ.md>
module Indigo.Core.History
  ( History
  , Action (..)

    -- * Create
  , empty

    -- * Query
  , before
  , now
  , after
  , isEpoch
  , isPast
  , isPresent
  , length

    -- * Modify
  , act
  , shiftEarlier
  , shiftLater
  , shiftEpoch
  , shiftPresent
  )
where

import BasePrelude (until)
import Data.Default.Class (Default (..))
import Data.List qualified as List
import Prelude hiding (empty, length)

-- TODO: Explain
data History a = History
  { before :: ![a]
  , after :: ![a]
  }
  deriving stock (Show, Eq)

instance Default (History a) where
  def :: History a
  def = empty

-- | Laws:
-- `x != invert x` or `id != invert`
-- `x = invert (invert x)` or `id = invert . invert`
class Action a where
  invert :: a -> a

empty :: History a
empty =
  History
    { before = []
    , after = []
    }

-- | Actions occurring before the frame of reference, from newest to oldest.
before :: History a -> [a]
before history = history.before

-- | Action that just occurred from the frame of reference.
now :: History a -> Maybe a
now history = viaNonEmpty head history.before

-- | Actions occurring after the frame of reference, from oldest to newest.
after :: History a -> [a]
after history = history.after

-- | Whether the frame of reference is at the epoch (beginning of history).
isEpoch :: History a -> Bool
isEpoch history = null history.before

-- | Whether the frame of reference is in the past.
isPast :: History a -> Bool
isPast = not . isPresent

-- | Whether the frame of reference is in the present (end of history).
isPresent :: History a -> Bool
isPresent history = null history.after

-- | Number of actions in the history, irrespective of the frame of reference.
length :: History a -> Int
length history = List.length history.before + List.length history.after

-- TODO: Explain
act :: Action a => a -> History a -> History a
act action history =
  if null history.after then
    history{ before = action : history.before }
  else
    history
      { before = action : before
      , after = []
      }
    where
    before =
      mconcat
        [ fmap invert history.after
        , reverse history.after
        , history.before
        ]

-- | Shift the frame of reference one action earlier.
shiftEarlier :: History a -> History a
shiftEarlier history =
  case history.before of
    [] ->
      history
    action : actions ->
      history
        { before = actions
        , after = action : history.after
        }

-- | Shift the frame of reference one action later.
shiftLater :: History a -> History a
shiftLater history =
  case history.after of
    [] ->
      history
    action : actions ->
      history
        { before = action : history.before
        , after = actions
        }

-- | Shift the frame of reference to the epoch (beginning of history).
shiftEpoch :: History a -> History a
shiftEpoch = until isEpoch shiftEarlier

-- | Shift the frame of reference to the present (end of history).
shiftPresent :: History a -> History a
shiftPresent = until isPresent shiftLater
