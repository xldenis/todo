{-# LANGUAGE OverloadedStrings #-}
module Create where

import Protolude

import Graphics.Vty (Event(..), Key(..))
import Graphics.Vty.Attributes (defAttr)

import Brick.Types as B
import Brick

import Todo.Syntax

data Tree a = Tree [Todo a] Int deriving (Show)

tree :: [Todo a] -> Tree a
tree t = Tree t 0

moveTree :: (Int -> Int) -> Tree a -> Tree a
moveTree f (Tree ts i) = Tree ts (f i)

moveTreeUp :: Tree a -> Tree a
moveTreeUp = moveTree (+ 1)

moveTreeDown :: Tree a -> Tree a
moveTreeDown = moveTree (\a -> a - 1)

app = App
  { appDraw = pure . ui'
  , appChooseCursor = neverShowCursor
  , appHandleEvent = appEventHandler
  , appStartEvent = return
  , appAttrMap = const $ attrMap defAttr []
  }

appEventHandler :: Tree a -> BrickEvent n e -> EventM t (Next (Tree a))
appEventHandler s (VtyEvent e) = do
  if isExit e then
    halt s
  else
    continue =<< handleTreeEvent e s
appEventHandler s _ = continue s
isExit (EvKey (KChar 'q') []) = True
isExit _ = False

handleTreeEvent :: Event -> Tree a -> EventM n (Tree a)
handleTreeEvent e t =
  case e of
    EvKey KUp []   -> return $ moveTreeUp t
    EvKey KDown [] -> return $ moveTreeDown t
    _ -> return t

ui' :: Tree Task -> Widget ()
ui' (Tree ts off) = viewport () Vertical . translateBy (B.Location (0, off)) $  vBox $ map renderTodo ts

renderTask :: Task -> Widget ()
renderTask = visible . str . prettyPrint

renderTodo :: Todo Task -> Widget ()
renderTodo t =
  Widget Greedy Fixed $ do
    let subT = padLeft (Pad 2) . vBox $ map renderTodo (subTasks t)
    render $ str "- " <+> renderTask (task t) <=> subT
