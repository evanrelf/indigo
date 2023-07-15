{-# LANGUAGE NoFieldSelectors #-}

-- https://github.com/zaboople/klonk/blob/master/TheGURQ.md
module Indigo.Core.History
  ( History

    -- * Create
  , empty

    -- * Query
  , past
  , future

    -- * Modify
  , act
  , travelForward
  , travelBackward
  )
where

import Data.Default.Class (Default (..))
import Data.Sequence (Seq (..))
import Prelude hiding (empty, reverse)

import qualified Data.Sequence as Seq

data History a = History
  { past :: Seq a
  , future :: Seq a
  }

instance Default (History a) where
  def :: History a
  def = empty

-- `x = reverse (reverse x)` or `id = reverse . reverse`
class Reversible a where
  reverse :: a -> a

empty :: History a
empty =
  History
    { past = Seq.empty
    , future = Seq.empty
    }

past :: History a -> Seq a
past history = history.past

future :: History a -> Seq a
future history = history.future

act :: Reversible a => a -> History a -> History a
act action history =
  if Seq.null history.future then
    history{ past = history.past :|> action }
  else
    history
      { past = (history.past <> history.future <> fmap reverse history.future) :|> action
      , future = Seq.empty
      }

travelBackward :: History a -> History a
travelBackward history =
  case history.past of
    Empty ->
      history
    actions :|> action ->
      history
        { past = actions
        , future = action :<| history.future
        }

travelForward :: History a -> History a
travelForward history =
  case history.future of
    Empty ->
      history
    action :<| actions ->
      history
        { past = history.past :|> action
        , future = actions
        }
