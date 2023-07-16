module Indigo.Core.Mode
  ( Mode (..)
  , NormalMode (..)
  , GotoMode (..)
  , CommandMode (..)
  , InsertMode (..)
  )
where

import Data.Default.Class (Default (..))

data Mode
  = Normal !NormalMode
  | Goto !GotoMode
  | Command !CommandMode
  | Insert !InsertMode

instance Default Mode where
  def :: Mode
  def = Normal def

data NormalMode = NormalMode
  { count :: {-# UNPACK #-} !Word
  }

instance Default NormalMode where
  def :: NormalMode
  def =
    NormalMode
      { count = 0
      }

data GotoMode = GotoMode

instance Default GotoMode where
  def :: GotoMode
  def = GotoMode

data CommandMode = CommandMode
  { commandLine :: !()
  }

instance Default CommandMode where
  def :: CommandMode
  def =
    CommandMode
      { commandLine = ()
      }

data InsertMode = InsertMode

instance Default InsertMode where
  def :: InsertMode
  def = InsertMode
