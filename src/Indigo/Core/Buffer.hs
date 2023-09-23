module Indigo.Core.Buffer
  ( Buffer (..)
  , InMemoryBuffer
  , FileBuffer
  )
where

import Indigo.Core.Buffer.File (FileBuffer)
import Indigo.Core.Buffer.InMemory (InMemoryBuffer)

data Buffer
  = InMemory !InMemoryBuffer
  | File !FileBuffer
