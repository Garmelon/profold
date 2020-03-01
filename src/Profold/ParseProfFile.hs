{-# LANGUAGE OverloadedStrings #-}

module Profold.ParseProfFile
  ( ProfFile(..)
  , parseProfFile
  ) where

import           Control.Monad
import qualified Data.Text            as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Profold.LineNode

data ProfFile = ProfFile
  { profInfoLines :: [T.Text]
  , profNode      :: LineNode
  } deriving (Show)

type Parser = Parsec Void T.Text

line :: Parser T.Text
line = label "line" $ takeWhileP Nothing (/= '\n') <* newline

skipUntilTheInterestingStuff :: Parser ()
skipUntilTheInterestingStuff = void $ skipManyTill line newLines
  where
    newLines = try $ newline >> newline

lineWithIndentation :: T.Text -> Parser T.Text
lineWithIndentation indentation = do
  void $ string indentation
  l <- line
  pure $ indentation <> l

lineNode :: T.Text -> Parser LineNode
lineNode indentation = do
  text <- lineWithIndentation indentation
  children <- many $ lineNode $ indentation <> " "
  pure $ newLineNode text children

profFile :: Parser ProfFile
profFile = do
  skipUntilTheInterestingStuff
  info <- count 2 line
  void newline
  node <- lineNode ""
  pure $ ProfFile info node

parseProfFile :: String -> T.Text -> Either String ProfFile
parseProfFile filename text = case parse profFile filename text of
  Left e   -> Left $ errorBundlePretty e
  Right f -> Right f
