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
import qualified Data.Vector      as V

import           Profold.LineNode
import           Profold.Util

data UiState = UiState
  { uiTree    :: LineNode
  , uiFocused :: Path
  } deriving (Show)

newUiState :: LineNode -> UiState
newUiState ln = UiState ln []

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

wrapFolded :: Bool -> Widget n -> Widget n
wrapFolded False widget = withDefAttr "unfolded" $ str " " <+> widget
wrapFolded True widget  = withDefAttr "folded" $ str "+" <+> widget

wrapFocused :: Bool -> Widget n -> Widget n
wrapFocused False = withDefAttr "unfocused"
wrapFocused True  = visible . withDefAttr "focused"

renderLine :: Bool -> LineNode -> Widget n
renderLine focused ln =
  wrapFocused focused $
  wrapFolded foldedWithChildren $ padRight Max $ txt $ lineText ln
  where
    foldedWithChildren = lineFolded ln && not (V.null $ lineChildren ln)

renderUiState :: UiState -> Widget n
renderUiState s =
  let flat = V.toList $ flatten $ uiTree s
      focused = uiFocused s
      rendered = map (\(p, ln) -> renderLine (p == focused) ln) flat
  in  vBox rendered
