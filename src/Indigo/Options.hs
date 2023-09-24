{-# LANGUAGE ApplicativeDo #-}

module Indigo.Options
  ( Options (..)
  , getOptions
  )
where

import Options.Applicative qualified as Options

data Options = Options
  { files :: ![FilePath]
  }

getOptions :: MonadIO m => m Options
getOptions = liftIO do
  Options.execParser $ Options.info (Options.helper <*> parseOptions) mempty

parseOptions :: Options.Parser Options
parseOptions = do
  files <-
    many . Options.strArgument . mconcat $
      [ Options.metavar "FILE"
      ]

  pure Options{ files }
