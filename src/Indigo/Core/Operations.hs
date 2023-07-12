module Indigo.Core.Operations
  ( Operations
  , invert
  , apply
  )
where

import Data.Group (Group (..))
import Data.Sequence (Seq (..))
import Indigo.Core.Operation (Operation)
import Indigo.Core.Rope (Rope)
import Prelude hiding (rights, lefts)

import qualified Indigo.Core.Operation as Operation

newtype Operations = Operations{ unOperations :: Seq Operation }
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

instance Group Operations where
  invert :: Operations -> Operations
  invert (Operations operations) = Operations (fmap Operation.invert operations)

apply :: Operations -> Rope -> Rope
apply (Operations operations) rope =
  foldl' (flip Operation.apply) rope operations
