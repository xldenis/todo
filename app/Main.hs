{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Protolude

import           Options.Generic
import           Text.Megaparsec

import qualified Brick.Main      as M

import           Create
import           Todo

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

type Action = [Todo Task] -> IO [Todo Task]

applyAction :: Command -> IO ()
applyAction command = do
  res <- parseFromFile todo "todo"
  case res of
    Left err -> putStrLn (parseErrorPretty err)
    Right todos -> do
     todos' <- applyCommand command todos
     writeFile "todo" (toS $ prettyPrint todos')
  where applyCommand (Show _)   todos = showTasks todos
        applyCommand (Current)  todos = currentTasks todos
        applyCommand (Finish)   todos = finishTask todos
        applyCommand (Next)     todos = nextTask todos
        applyCommand (Create t) todos = createTask todos

createTask :: Action
createTask st = do
  M.defaultMain app (tree st)
  return st

showTasks :: Action
showTasks st = do
  putStrLn $ prettyPrint st
  return st

-- createTask   :: Text -> Action

finishTask :: Action
finishTask st = do
  let opened = join $ map (filterTask $ \t -> status t == Started) st
  let valid  = filter (foldr (\ t acc -> acc && (status t == Finished)) True) opened
  let flattened = join $ map flattenTask valid

  putStrLn . prettyPrint $ numed flattened

  (id :: Maybe Int) <- fmap (readMaybe . toS) getLine

  case id >>= (\i -> (,) <$> pure i <*> flattened `safeIndex` (i - 1)) of
    Just (i, selected) -> do
      let updated = sequenceA . foreach st $ \tds -> fmap (\t -> t { status = Finished}) <$> treeAt tds selected
      putStrLn . prettyPrint $ updated
      putStrLn . prettyPrint . Numed i . pure $ selected

      return $ asum updated
    Nothing -> putText "not a valid index" *> return st

safeIndex :: [a] -> Int -> Maybe a
safeIndex a i = case drop i a of
  x:_ -> pure x
  []  -> empty

nextTask :: Action
nextTask st = do
  let avail = head $ map firstOpenTask st

  putStrLn . prettyPrint $ avail

  return st

firstOpenTask :: Todo Task -> Maybe Task
firstOpenTask (Todo t _ ) | status t == Finished = empty
firstOpenTask (Todo t []) = pure t
firstOpenTask (Todo t ts) | isOpened t = asum (map firstOpenTask ts) <|> pure t
firstOpenTask (Todo _ ts) = asum (map firstOpenTask ts)

isOpened :: Task -> Bool
isOpened (Task Open _ _ _) = True
isOpened _                 = False

currentTasks :: Action
currentTasks st = do
  let opened = join $ map (filterTask $ \t -> status t == Started) st
  putStrLn $ prettyPrint opened
  return st

