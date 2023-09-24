{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | <https://github.com/zaboople/klonk/blob/master/TheGURQ.md>
module Indigo.Core.History
  ( History
  , Action (..)

    -- * Create
  , empty

    -- * Query
  , past
  , future
  , length

    -- * Modify
  , act
  , travelBackward
  , travelForward
  )
where

import Data.Default.Class (Default (..))
import Data.List qualified as List
import Prelude hiding (empty, length)

data History a = History
  { past :: ![a]
  , future :: ![a]
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
    { past = []
    , future = []
    }

-- | Past actions, from newest to oldest
past :: History a -> [a]
past history = history.past

-- | Future actions, from oldest to newest
future :: History a -> [a]
future history = history.future

length :: History a -> Int
length history = List.length history.past + List.length history.future

act :: Action a => a -> History a -> History a
act action history =
  if null history.future then
    history{ past = action : history.past }
  else
    history
      { past = action : past
      , future = []
      }
    where
    past =
      mconcat
        [ fmap invert history.future
        , reverse history.future
        , history.past
        ]

travelBackward :: History a -> History a
travelBackward history =
  case history.past of
    [] ->
      history
    action : actions ->
      history
        { past = actions
        , future = action : history.future
        }

travelForward :: History a -> History a
travelForward history =
  case history.future of
    [] ->
      history
    action : actions ->
      history
        { past = action : history.past
        , future = actions
        }
