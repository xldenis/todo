{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
module Todo.Syntax where
import Protolude

import Data.String (String)

import Text.PrettyPrint.Free
import Text.PrettyPrint.Free.Internal (Doc(..))

import Data.Time

type TaskId = Int

data Task = Task
  { status :: Status
  , message :: Text
  , startedAt :: Maybe UTCTime
  , references :: [TaskId]
  , subTasks :: Todo
  } deriving (Show, Eq)

type Todo = [Task]

data Status = Started | Finished | Open
  deriving (Show, Eq)

data Numed = Numed Int [Task]

numed = Numed 1

instance Pretty Numed where
  pretty (Numed i ts) = vcat . foreach (naturals `zip` ts) $  \(i, t) -> pretty i <> (char '.') <+> pretty t
    where naturals :: [Int]
          naturals = i : map (+1) naturals

instance Pretty Task where
  pretty (Task {..}) = nest 2 $ pretty status <+> pretty message <+> prettyTime startedAt `above'` pretty subTasks
    where prettyTime (Just t) = brackets . text $ formatTime defaultTimeLocale rfc822DateFormat t
          prettyTime Nothing  = empty
          above' a Empty  = a
          above' a b = a `above` b
  prettyList tasks = vcat (map (\t -> text "-" <+> pretty t) tasks)

instance Pretty Status where
  pretty Started  = brackets (text "O")
  pretty Finished = brackets (text "X")
  pretty Open     = brackets (text " ")

prettyPrint :: Pretty a => a -> String
prettyPrint d = displayS (renderPretty 1.0 120 $ pretty d) ""

filterTask :: (Task -> Bool) -> Task -> [Task]
filterTask f t | f t = pure $ t { subTasks = (join $ map (filterTask f) $ subTasks t) }
filterTask _ _ = []

flattenTask :: Task -> [Task]
flattenTask t@(Task _ _ _ _ []) = [t]
flattenTask t@(Task _ _ _ _ ts) = t { subTasks = [] } : (join $ map flattenTask ts)

mapTask :: (Task -> Task) -> Task -> Task
mapTask f t@(Task _ _ _ _ []) = f t
mapTask f t@(Task _ _ _ _ ts) = f $ t { subTasks = map (mapTask f) ts }

pluckTask :: Int -> [Task] -> Maybe Task
pluckTask i ts = snd $ go i ts
  where go i t = foldr (\n o ->
          case fst o of
            j | j == i     -> o
            j | j == i - 1 -> (i, Just n { subTasks = []})
            j              -> go (i - j - 1) (subTasks n)
          ) (0, Nothing) t
