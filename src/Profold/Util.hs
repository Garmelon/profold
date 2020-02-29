module Profold.Util
  ( modifyAtIndex
  ) where

import qualified Data.Vector as V

modifyAtIndex :: Int -> (a -> a) -> V.Vector a -> V.Vector a
-- Yes, this function looks ugly, but it's short enough that I don't care.
modifyAtIndex i f v = maybe v (\a -> v V.// [(i, f a)]) (v V.!? i)
