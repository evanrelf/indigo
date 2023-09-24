{-# LANGUAGE ApplicativeDo #-}

module Indigo.Core.Command
  ( help
  )
where

import Options.Applicative qualified as Options
import Options.Applicative.Extra qualified as Options (helperWith)
import Prelude hiding (void)

-- TODO: Use `optparse-applicative`'s completion support

help :: Maybe Options.ParserPrefs -> Options.Parser a -> Text -> [Text] -> Text
help mPrefs parser0 name args0 = do
  let prefs = fromMaybe Options.defaultPrefs mPrefs
  let helpName = "INDIGO-INTERNAL-HELP"
  let parser = Options.helperWith (Options.long helpName) <*> (parser0 *> empty)
  let info = Options.info parser mempty
  let args = (toString <$> args0) <> ["--" <> helpName]
  let result = Options.execParserPure prefs info args
  case result of
    Options.Success void -> absurd void
    Options.CompletionInvoked _ -> error "unreachable"
    Options.Failure failure -> do
      let (helpText, _exitCode) = Options.renderFailure failure (toString name)
      toText helpText

_example :: Options.Parser ()
_example = do
  _ :: String <-
    Options.strOption . mconcat $
      [ Options.long "first-name"
      , Options.help "First name"
      ]
  _ :: Maybe String <-
    Options.optional . Options.strOption . mconcat $
      [ Options.long "last-name"
      , Options.help "Last name"
      ]
  _ :: Bool <-
    Options.switch . mconcat $
      [ Options.long "likes-dogs"
      , Options.help "Likes dogs"
      ]
  _ :: String <-
    Options.strArgument . mconcat $
      [ Options.metavar "PASSWORD"
      , Options.help "Password"
      ]
  pure ()
