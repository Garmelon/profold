{-# LANGUAGE OverloadedStrings #-}

module Profold.UiState
  ( UiState
  , newUiState
  -- * Modifying
  , moveFocusUp
  , moveFocusDown
  , toggleFold
  -- * Drawing
  , renderUiState
  ) where

import           Brick
import           Data.Maybe
import qualified Data.Text        as T
import qualified Data.Vector      as V

import           Profold.LineNode
import           Profold.Util

data UiState = UiState
  { uiInfo    :: [T.Text]
  , uiTree    :: LineNode
  , uiFocused :: Path
  } deriving (Show)

newUiState :: [T.Text] -> LineNode -> UiState
newUiState info ln = UiState info ln []

moveFocusUp :: UiState -> UiState
moveFocusUp s =
  fromMaybe s $ do
    prev <-
      fst <$> findSurrounding (\a -> fst a == uiFocused s) $
      flatten $ uiTree s
    pure s {uiFocused = fst prev}

moveFocusDown :: UiState -> UiState
moveFocusDown s =
  fromMaybe s $ do
    prev <-
      snd <$> findSurrounding (\a -> fst a == uiFocused s) $
      flatten $ uiTree s
    pure s {uiFocused = fst prev}

toggleFold :: UiState -> UiState
toggleFold s =
  s {uiTree = modify toggleFoldIfHasChildren (uiFocused s) (uiTree s)}
  where
    toggleFoldIfHasChildren ln
      | V.null $ lineChildren ln = ln
      | otherwise = ln {lineFolded = not $ lineFolded ln}

wrapFocused :: Bool -> Widget n -> Widget n
wrapFocused False = withDefAttr "unfocused"
wrapFocused True  = visible . withDefAttr "focused"

renderLine :: Bool -> LineNode -> Widget n
renderLine focused ln =
  let prefix
        | V.null $ lineChildren ln = " "
        | lineFolded ln = "+"
        | otherwise = "-"
  in  wrapFocused focused $ padRight Max $ str prefix <+> txt (lineText ln)

renderUiState :: (Show n, Ord n) => n -> UiState -> Widget n
renderUiState n s =
  let info = vBox $ map txt $ uiInfo s
      flat = V.toList $ flatten $ uiTree s
      focused = uiFocused s
      rendered = map (\(p, ln) -> renderLine (p == focused) ln) flat
  in  info <=> viewport n Vertical (vBox rendered)
