module Indigo (main) where

import Data.Default.Class (Default (..))
import Indigo.Core.Buffer qualified as Buffer
import Indigo.Core.Buffer.File qualified as FileBuffer
import Indigo.Core.Editor qualified as Editor
import Indigo.Tui qualified as Tui

main :: IO ()
main = do
  getArgs >>= \case
    [] -> do
      Tui.run def
    [path] -> do
      buffer <- Buffer.File <$> FileBuffer.fromFile path
      Tui.run (Editor.addBuffer buffer def)
    _ -> do
      die "usage: indigo [PATH]"
