module Profold.LineNode
  ( LineNode(..)
  , newLineNode
  , Path
  , flatten
  , modify
  ) where

import qualified Data.Text    as T
import qualified Data.Vector  as V

import           Profold.Util

data LineNode = LineNode
  { lineText     :: T.Text
  , lineChildren :: V.Vector LineNode
  , lineFolded   :: Bool
  } deriving (Show)

newLineNode :: T.Text -> [LineNode] -> LineNode
newLineNode text children = LineNode text (V.fromList children) True

type Path = [Int]

flatten :: LineNode -> V.Vector (Path, LineNode)
flatten ln
  | lineFolded ln = V.singleton ([], ln)
  | otherwise =
    V.cons ([], ln) $
    V.imap (\i (is, n) -> (i : is, n)) $ V.concatMap flatten $ lineChildren ln

modify :: (LineNode -> LineNode) -> Path -> LineNode -> LineNode
modify f [] ln = f ln
modify f (i:is) ln =
  ln {lineChildren = modifyAtIndex i (modify f is) $ lineChildren ln}
