module Reckon.Parser.Data where

import Reckon.Syntax
import Reckon.Lexer
import Text.Megaparsec

--parse the integer
int :: Parser Expr 
int = Literal  <$> int' 

int' :: Parser Lit
int' = RInteger <$> integer <?> "int"

--parse the float
floating :: Parser Expr
floating = Literal  <$> floating'

floating' :: Parser Lit
floating' = RFloat <$> float <?> "float"

rchar :: Parser Expr
rchar = Literal <$> rchar'

rchar' :: Parser Lit
rchar' = RChar <$> charLit <?> "char"

rstring :: Parser Expr
rstring = Literal <$> rstring'

rstring' :: Parser Lit 
rstring' = RString <$> stringLit <?> "string"

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
