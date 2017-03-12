module Todo (module X, parseFromFile) where

import Protolude

import Todo.Parser as X
import Todo.Syntax as X

import Text.Megaparsec (runParser, ParseError, Dec)
import Text.Megaparsec.Text

parseFromFile :: Parser a -> FilePath -> IO (Either (ParseError Char Dec) a)
parseFromFile p file = runParser p file <$> readFile file
