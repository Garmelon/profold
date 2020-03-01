{-# LANGUAGE OverloadedStrings #-}

module Profold.App
  ( UiState
  , newUiState
  , myApp
  ) where

import           Brick
import           Brick.Widgets.List
import qualified Data.Text          as T
import qualified Data.Vector        as V
import qualified Graphics.Vty       as Vty

import           Profold.LineNode

data UiName = UiList
  deriving (Show, Eq, Ord)

data UiState = UiState
  { uiInfo :: [T.Text]
  , uiTree :: LineNode
  , uiList :: List UiName (Path, LineNode)
  } deriving (Show)

newUiState :: [T.Text] -> LineNode -> UiState
newUiState info ln = UiState info ln $ list UiList (flatten ln) 1

toggleFold :: UiState -> UiState
toggleFold s = case listSelectedElement (uiList s) of
  Nothing -> s
  Just (i, (path, _)) ->
    let newTree = modify toggleIfHasChildren path $ uiTree s
        newList = listReplace (flatten newTree) (Just i) (uiList s)
    in  s {uiTree = newTree, uiList = newList}
  where
    toggleIfHasChildren ln
      | V.null $ lineChildren ln = ln
      | otherwise = ln {lineFolded = not $ lineFolded ln}

renderLine :: Bool -> (Path, LineNode) -> Widget UiName
renderLine focused (_, ln)
  | focused = visible $ withDefAttr "focused" widget
  | otherwise = widget
  where
    prefix
      | V.null $ lineChildren ln = " "
      | lineFolded ln = "+"
      | otherwise = "-"
    widget = padRight Max $ str prefix <+> txt (lineText ln)

renderUiState :: UiState -> Widget UiName
renderUiState s =
  let info = vBox $ map (\t -> str " " <+> txt t) $ uiInfo s
  in  info <=> renderList renderLine True (uiList s)

{- The actual App -}

myAppDraw :: UiState -> [Widget UiName]
myAppDraw s = [renderUiState s]

myHandleEvent :: UiState -> BrickEvent UiName e -> EventM UiName (Next UiState)
myHandleEvent s (VtyEvent (Vty.EvKey Vty.KEsc _))         = halt s
myHandleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'q') _))  = halt s
myHandleEvent s (VtyEvent (Vty.EvKey (Vty.KChar '\t') _)) = continue $ toggleFold s
myHandleEvent s (VtyEvent e) = do
  newList <- handleListEventVi handleListEvent e (uiList s)
  continue s{uiList = newList}
myHandleEvent s _ = continue s

myAttrMap :: AttrMap
myAttrMap = attrMap Vty.defAttr
  [ ("focused", Vty.defAttr `Vty.withStyle` Vty.reverseVideo)
  ]

myApp :: App UiState () UiName
myApp = App
  { appDraw         = myAppDraw
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = myHandleEvent
  , appStartEvent   = pure
  , appAttrMap      = const myAttrMap
  }
