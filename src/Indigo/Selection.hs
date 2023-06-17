module Indigo.Selection
  ( Selection (Selection, above, primary, below)
  , fromRange
  , fromRanges
  , mkSelection
  , primary
  , above
  , below
  , insert
  , toRanges
  )
where

import Indigo.Selection.Internal
