{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE TupleSections #-}

module Reckon.Parser where 

import Reckon.Syntax
import Reckon.Lexer
import Reckon.Parser.Decl
import Reckon.Parser.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

statement :: Parser Stmt
statement = (D <$> declare)
        <|> (E <$> expr)


contens :: Parser a -> Parser a
contens p = do
  space
  r <- p
  eof
  return r

topLevel :: Parser [Stmt]
topLevel = many $ do
  stmt <- statement
  return stmt

parseStatement s = parse  (contens statement) "<stdin>" s 

parseTopLevel s = parse  (contens topLevel) "<stdin>" s