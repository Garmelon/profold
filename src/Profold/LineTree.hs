module Profold.LineTree
  ( LineNode(..)
  , Path
  , onlyUnfolded
  , flatten
  , modify
  , toggleFold
  ) where

import qualified Data.Text    as T
import qualified Data.Vector  as V

import           Profold.Util

data LineNode = LineNode
  { lineText     :: T.Text
  , lineChildren :: V.Vector LineNode
  , lineFolded   :: Bool
  } deriving (Show)

type Path = [Int]

onlyUnfolded :: LineNode -> LineNode
onlyUnfolded ln
  | lineFolded ln = ln{lineChildren = V.empty}
  | otherwise     = ln

flatten :: LineNode -> V.Vector (Path, T.Text)
flatten ln =
  V.cons ([], lineText ln) $
  V.imap (\i (is, t) -> (i : is, t)) $
  V.concatMap flatten $
  lineChildren ln

modify :: (LineNode -> LineNode) -> Path -> LineNode -> LineNode
modify f [] ln = f ln
modify f (i:is) ln = ln{lineChildren = modifyAtIndex i (modify f is) $ lineChildren ln}

toggleFold :: Path -> LineNode -> LineNode
toggleFold = modify $ \ln -> ln{lineFolded = not $ lineFolded ln}
