module Profold.Util
  ( modifyAtIndex
  , findSurrounding
  ) where

import           Data.Maybe
import qualified Data.Vector as V

modifyAtIndex :: Int -> (a -> a) -> V.Vector a -> V.Vector a
-- Yes, this function looks ugly, but it's short enough that I don't care.
modifyAtIndex i f v = maybe v (\a -> v V.// [(i, f a)]) (v V.!? i)

findSurrounding :: (a -> Bool) -> V.Vector a -> (Maybe a, Maybe a)
findSurrounding f v = fromMaybe (Nothing, Nothing) $ do
  i <- V.findIndex f v
  pure (v V.!? (i - 1), v V.!? (i + 1))
