-- | A module for parsing command line options.

module Profold.Options
  ( Options(..)
  , options
  ) where

import Options.Applicative

-- | The command line options.
newtype Options = Options
  { optFileName :: String
  -- ^ Name of the @.prof@ file to open and parse.
  } deriving (Show)

parser :: Parser Options
parser = Options
  <$> strArgument (help "Path to the .prof file" <> metavar "FILE")

-- | A parser for the command line options.
options :: ParserInfo Options
options = info (helper <*> parser) fullDesc
