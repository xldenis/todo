{-# LANGUAGE RecordWildCards, DeriveAnyClass, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
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
  } deriving (Show, Eq)

data Todo a = Todo { task :: a, subTasks :: [Todo a]}
  deriving (Show, Eq, Functor, Applicative, Foldable, Traversable)

data Status = Started | Finished | Open
  deriving (Show, Eq)

data Numed = Numed Int [Task]

numed :: [Task] -> Numed
numed = Numed 1

instance Pretty Numed where
  pretty (Numed i ts) = vcat . foreach (naturals `zip` ts) $  \(i, t) -> pretty i <> (char '.') <+> pretty t
    where naturals :: [Int]
          naturals = i : map (+1) naturals


instance Pretty a => Pretty (Todo a) where
  pretty (Todo {..}) = nest 2 $ text "-" <+> pretty task `above'`
    vcat (map (\t -> pretty t) subTasks)
    where above' a Empty  = a
          above' a b = a `above` b
  prettyList ts = vcat $ map pretty ts
instance Pretty Task where
  pretty (Task {..}) = pretty status <+> pretty message <+> prettyTime startedAt
    where prettyTime (Just t) = brackets . text $ formatTime defaultTimeLocale rfc822DateFormat t
          prettyTime Nothing  = empty

instance Pretty Status where
  pretty Started  = brackets (text "O")
  pretty Finished = brackets (text "X")
  pretty Open     = brackets (text " ")

prettyPrint :: Pretty a => a -> String
prettyPrint d = displayS (renderPretty 1.0 120 $ pretty d) ""
