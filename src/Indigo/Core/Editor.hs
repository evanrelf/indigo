module Indigo.Core.Editor
  ( Editor
  )
where

import Indigo.Core.Buffer (Buffer)

data Editor = Editor
  { buffers :: !(NonEmpty Buffer)
  }
