-- | A module containing the tree data structure for lines in the @.prof@ file
-- and some useful functions.

module Profold.LineNode
  ( LineNode(..)
  , newLineNode
  , Path
  , flatten
  , modify
  ) where

import qualified Data.Text   as T
import qualified Data.Vector as V

-- | A line from the @.prof@ file and its children.
data LineNode = LineNode
  { lineText     :: T.Text
  -- ^ The complete text of the line, including its indentation.
  , lineChildren :: V.Vector LineNode
  -- ^ The line's children.
  , lineFolded   :: Bool
  -- ^ Whether the line's children are folded away.
  } deriving (Show)

-- | Create a new 'LineNode'.
newLineNode :: T.Text -> [LineNode] -> LineNode
newLineNode text children = LineNode text (V.fromList children) True

-- | A path (similar to a file path) referencing a node inside a 'LineNode'
-- tree.
type Path = [Int]

-- | Convert a 'LineNode' into a flat list of itself and all visible children.
-- The list includes the 'Path' to each node. Nodes that would not be visible
-- because one of their parents is folded are not included.
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

-- | Modify a node at a specific location in the tree. Returns the original node
-- if no node exists at that path.
modify :: (LineNode -> LineNode) -> Path -> LineNode -> LineNode
modify f [] ln = f ln
modify f (i:is) ln =
  ln {lineChildren = modifyAtIndex i (modify f is) $ lineChildren ln}
