module Reckon.Parser.Data where

import Reckon.Syntax
import Reckon.Lexer
import Text.Megaparsec

--parse the integer
int :: Parser Expr 
int = Literal  . RInteger <$> integer

int' :: Parser Lit
int' = RInteger <$> integer

--parse the float
floating :: Parser Expr
floating = Literal  . RFloat <$> float

floating' :: Parser Lit
floating' = RFloat <$> float

rchar :: Parser Expr
rchar = Literal . RChar <$> charLit

rchar' :: Parser Lit
rchar' = RChar <$> charLit

rstring' :: Parser Lit 
rstring' = RString <$> stringLit

rstring :: Parser Expr
rstring = Literal . RString <$> stringLit <?> "string"

lit :: Parser Lit
lit  = try int'
   <|> try floating'
   <|> try rchar'
   <|> try list'
   <|> try rstring'


llit :: Parser Lit
llit = do
  reservedOp ","
  l <- lit
  return l


list :: Parser Expr
list = Literal <$> list'

list' :: Parser Lit --bug:wait fix
list' = brackets $ do
  h <- lit
  t <- many llit
  return $ List ([h] ++ t)
