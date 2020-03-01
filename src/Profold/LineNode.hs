module Profold.LineNode
  ( LineNode(..)
  , newLineNode
  , Path
  , flatten
  , modify
  ) where

import qualified Data.Text    as T
import qualified Data.Vector  as V

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
    V.concat $ V.toList $ V.imap flattenChild $ lineChildren ln
  where
    flattenChild :: Int -> LineNode -> V.Vector (Path, LineNode)
    flattenChild i c = V.map (\(is, n) -> (i : is, n)) $ flatten c

modifyAtIndex :: Int -> (a -> a) -> V.Vector a -> V.Vector a
-- Yes, this function looks ugly, but it's short enough that I don't care.
modifyAtIndex i f v = maybe v (\a -> v V.// [(i, f a)]) (v V.!? i)

modify :: (LineNode -> LineNode) -> Path -> LineNode -> LineNode
modify f [] ln = f ln
modify f (i:is) ln =
  ln {lineChildren = modifyAtIndex i (modify f is) $ lineChildren ln}
