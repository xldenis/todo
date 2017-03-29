module Todo.Parser where

import Protolude
import Prelude as P (String)

import           Text.Megaparsec
import           Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

import Data.Time
import Data.Text (strip)

import Todo.Syntax


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: P.String -> Parser P.String
symbol = L.symbol sc

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf (" \t" :: String)) lineComment empty

lineComment :: Parser ()
lineComment = char '#' *> skipMany (noneOf ("\n" :: String))

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

todo :: Parser [Todo Task]
todo = many (L.nonIndented scn indentedTask) <* eof

taskLine :: Parser Task
taskLine = do
  sts     <- lexeme $ brackets (lexeme statusSym)
  message <- lexeme $ many (noneOf ("\n[" :: String))
  time    <- optional . lexeme $ brackets $ do
    str <- many $ noneOf ("\n]" :: String)
    str2Time str
  return $ Task sts (strip $ toS message) time []
  where statusSym =     char 'X' *> pure Finished
                    <|> char 'O' *> pure Started
                    <|> char ' ' *> pure Open
        str2Time :: Monad m => String -> m UTCTime
        str2Time = parseTimeM False defaultTimeLocale rfc822DateFormat

indentedTask :: Parser (Todo Task)
indentedTask = L.indentBlock scn $ do
  curIdent <- L.indentLevel
  symbol "-"
  t <- taskLine

  let indent = L.IndentMany (Just $ curIdent <> unsafePos 2) (return . Todo t) (indentedTask)

  return indent


