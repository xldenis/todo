{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
module Main where

import Protolude
import Todo

import Text.Megaparsec

import Options.Generic

data Command
  = Current
  | Finish
  | Create Text
  | Next
  | Show { _filter :: Maybe Text }
  | Edit
  deriving (Show, Eq, Generic)

instance ParseRecord Command

main :: IO ()
main = do
  config <- getRecord "todo" :: IO Command
  applyAction config

type Action = Todo -> IO Todo

applyAction :: Command -> IO ()
applyAction command = do
  res <- parseFromFile todo "todo"
  case res of
    Left err -> putStrLn (parseErrorPretty err)
    Right todos -> do
     todos' <- applyCommand command todos
     writeFile "todo" (toS $ prettyPrint todos')
  where applyCommand (Show _)  todos = showTasks todos
        applyCommand (Current) todos = currentTasks todos
        applyCommand (Finish)  todos = finishTask todos
        applyCommand (Next)    todos = nextTask todos

showTasks :: Action
showTasks st = do
  putStrLn $ prettyPrint st
  return st

-- createTask   :: Text -> Action

finishTask :: Action
finishTask st = do
  let opened = join $ map (filterTask $ \t -> status t == Started) st
  let flattened = join $ map flattenTask opened

  putStrLn . prettyPrint $ numed flattened

  (id :: Maybe Int) <- liftM (readMaybe . toS) getLine

  case id >>= (\i -> (,) <$> pure i <*> pluckTask i opened) of
    Just (i, task) -> do
      let updated = map (mapTask (\t ->
                                  if (t { subTasks = []} == task)
                                  then mapTask (\j -> j { status = Finished }) t
                                  else t)) st
      putStrLn $ prettyPrint updated -- . Numed i . pure $ task
    Nothing -> mempty

  return st

nextTask :: Action
nextTask st = do
  let avail = head $ map firstOpenTask st

  putStrLn . prettyPrint $ avail

  return st

firstOpenTask :: Task -> Maybe Task
firstOpenTask t@(Task Open _ _ _ [])    = pure t
firstOpenTask t@(Task Open _ _ _ ts)    = asum (map firstOpenTask ts) <|> pure t
firstOpenTask   (Task Started _ _ _ ts) = asum $ map firstOpenTask ts
firstOpenTask _ = Nothing


currentTasks :: Action
currentTasks st = do
  let opened = join $ map (filterTask $ \t -> status t == Started) st
  putStrLn $ prettyPrint opened
  return st

