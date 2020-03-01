module Profold.Options
  ( Options(..)
  , options
  ) where

import Options.Applicative

newtype Options = Options
  { optFileName :: String
  } deriving (Show)

parser :: Parser Options
parser = Options
  <$> strArgument (help "Path to the .prof file" <> metavar "FILE")

options :: ParserInfo Options
options = info (helper <*> parser) fullDesc
