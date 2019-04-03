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

lit :: Parser Lit
lit  = try int'
   <|> try floating'
   <|> try rchar'
   <|> try list'

llit :: Parser Lit
llit = do
  l <- lit
  reservedOp ","
  return l

{- rstring' :: Parser Lit --bug:wait fix
rstring' = do
  s <- many rchar'
  return $ Array s

rstring :: Parser Expr
rstring = Literal <$> doubleQuote rstring' <?> "string" 
 -}
list :: Parser Expr
list = Literal <$> list'

list' :: Parser Lit --bug:wait fix
list' = brackets $ do
  a <- many lit
  return $ List a
