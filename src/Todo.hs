module Todo (module X, module Todo) where

import Protolude

import Todo.Parser as X
import Todo.Syntax as X

import Text.Megaparsec (runParser, ParseError, Dec)
import Text.Megaparsec.Text

parseFromFile :: Parser a -> FilePath -> IO (Either (ParseError Char Dec) a)
parseFromFile p file = runParser p file <$> readFile file

filterTask :: (Task -> Bool) -> Todo Task -> [Todo Task]
filterTask f t = case f (task t) of
  True -> pure $ t { subTasks = (join $ map (filterTask f) $ subTasks t) }
  False -> []
filterTask _ _ = []

flattenTask :: Todo a -> [a]
flattenTask ts = squish ts []
  where squish (Todo t ts) acc = t : foldr squish acc ts

pluckTask :: Int -> [Todo Task] -> Maybe Task
pluckTask i (ts:_) = case drop i (flattenTask ts) of
  x:_ -> pure x
  [] ->  Nothing

treeAt :: Eq a => Todo a -> a -> Maybe (Todo a)
treeAt t a | task t == a = pure t
treeAt t a = asum $ map (\s -> treeAt s a) (subTasks t)
