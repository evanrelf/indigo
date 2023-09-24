{-# LANGUAGE ApplicativeDo #-}

module Indigo.Core.Command
  ( Command (..)
  , run
  , help
  )
where

import Options.Applicative qualified as Options
import Options.Applicative.Extra qualified as Options (helperWith)
import Prelude hiding (void)

-- TODO: Use `optparse-applicative`'s completion support

-- TODO: Should this have an `extract :: o -> a` field to make the parse result
-- existential? If so, should it just return `a`, or something more constrained
-- like `s -> m s`? Should we put a `Show` constraint (or whatever) on `o` for
-- debugging? Not sure, just ideas.
data Command a = Command
  { name :: Text
  , parser :: Options.Parser a
  }
  deriving stock (Functor)

run :: Command a -> [Text] -> Either Text a
run Command{name, parser} args = do
  let info = Options.info parser mempty
  let result = Options.execParserPure parserPrefs info (toString <$> args)
  case result of
    Options.CompletionInvoked _ -> error "unreachable"
    Options.Success x -> Right x
    Options.Failure failure -> do
      let (helpText, _exitCode) = Options.renderFailure failure (toString name)
      Left $ toText helpText

help :: Command a -> [Text] -> Text
help Command{name, parser} args = help' Nothing parser name args

help' :: Maybe Options.ParserPrefs -> Options.Parser a -> Text -> [Text] -> Text
help' mPrefs parser0 name args0 = do
  let prefs = fromMaybe parserPrefs mPrefs
  let helpName = "INDIGO-INTERNAL-HELP"
  let parser = Options.helperWith (Options.long helpName) <*> (parser0 *> empty)
  let info = Options.info parser mempty
  let args = (toString <$> args0) <> ["--" <> helpName]
  let result = Options.execParserPure prefs info args
  case result of
    Options.CompletionInvoked _ -> error "unreachable"
    Options.Success void -> absurd void
    Options.Failure failure -> do
      let (helpText, _exitCode) = Options.renderFailure failure (toString name)
      toText helpText

parserPrefs :: Options.ParserPrefs
parserPrefs = Options.defaultPrefs

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
