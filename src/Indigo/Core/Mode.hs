module Indigo.Core.Mode
  ( Mode (..)
  , NormalMode (..)
  , GotoMode (..)
  , CommandMode (..)
  , InsertMode (..)
  )
where

data Mode
  = Normal NormalMode
  | Goto GotoMode
  | Command CommandMode
  | Insert InsertMode

data NormalMode = NormalMode
  { count :: {-# UNPACK #-} !Word
  }

data GotoMode = GotoMode

data CommandMode = CommandMode
  { commandLine :: !()
  }

data InsertMode = InsertMode
