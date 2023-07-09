module Indigo.Core.Editor
  ( Editor
  )
where

import Indigo.Core.Buffer (Buffer)
import Indigo.Core.Mode (Mode)

data Editor = Editor
  { buffers :: !(NonEmpty Buffer)
  , mode :: !Mode
  }
