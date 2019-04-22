module Reckon.Parser.Data where

import Reckon.Syntax
import Reckon.Lexer
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L hiding(lexeme,symbol)

literal :: Parser Expr
literal = try integer
      <|> try floating
      <|> try rchar
      <|> try rstring

--parse the integer
integer :: Parser Expr 
integer = Literal . RInteger <$> integerLit <?> "int"

--parse the float
floating :: Parser Expr
floating = Literal . RFloat <$> float <?> "float"

rchar :: Parser Expr
rchar = Literal . RChar <$> charLit <?> "char"

rstring :: Parser Expr
rstring = Literal . RString <$> stringLit <?> "string"

-- literal data value
charLit :: Parser Char
charLit = between (symbol "\'") (symbol "\'") L.charLiteral

stringLit :: Parser String
stringLit = char '\"' *> manyTill L.charLiteral (char '\"')

integerLit :: Parser Integer
integerLit = lexeme L.decimal

floatLit :: Parser Float
floatLit = lexeme L.float