module Main where

import           Brick
import           Control.Monad
import qualified Data.Text.IO          as T
import           Options.Applicative

import           Profold.App
import           Profold.Options
import           Profold.ParseProfFile

main :: IO ()
main = do
  opts <- execParser options
  let filename = optFileName opts
  text <- T.readFile filename
  case parseProfFile filename text of
    Left e     -> putStrLn e
    Right f -> void $ defaultMain myApp $ newUiState (profInfoLines f) (profNode f)
