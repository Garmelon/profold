{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Brick
import           Control.Monad
import qualified Graphics.Vty     as Vty

import           Profold.LineNode
import           Profold.UiState

data UiName = UiViewport
  deriving (Show, Eq, Ord)

myAppDraw :: UiState -> [Widget UiName]
myAppDraw s = [viewport UiViewport Vertical $ renderUiState s]

myHandleEvent :: UiState -> BrickEvent n e -> EventM n (Next UiState)
myHandleEvent s (VtyEvent (Vty.EvKey Vty.KEsc _))         = halt s
myHandleEvent s (VtyEvent (Vty.EvKey Vty.KUp _))          = continue $ moveFocusUp s
myHandleEvent s (VtyEvent (Vty.EvKey Vty.KDown _))        = continue $ moveFocusDown s
myHandleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'q') _))  = halt s
myHandleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'k') _))  = continue $ moveFocusUp s
myHandleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'j') _))  = continue $ moveFocusDown s
myHandleEvent s (VtyEvent (Vty.EvKey (Vty.KChar '\t') _)) = continue $ toggleFold s
myHandleEvent s  _                                        = continue s

myAttrMap :: AttrMap
myAttrMap = attrMap Vty.defAttr
  [ ("focused", Vty.defAttr `Vty.withStyle` Vty.reverseVideo)
  ]

myApp :: App UiState () UiName
myApp = App
  { appDraw         = \s -> [renderUiState s]
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = myHandleEvent
  , appStartEvent   = pure
  , appAttrMap      = const myAttrMap
  }

main :: IO ()
main = void $ defaultMain myApp $ newUiState $ newLineNode "Hello world"
  [ newLineNode " Child" []
  , newLineNode " More children" []
  ]
