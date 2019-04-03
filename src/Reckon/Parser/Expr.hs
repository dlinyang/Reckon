{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE TupleSections #-}
module Reckon.Parser.Expr where

import Reckon.Syntax
import Reckon.Lexer
import Reckon.Parser.Data
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L


expr :: Parser Expr
expr = try application
   <|> try infixApp
   <|> term
   <|> lambda
   <|> letExpr
   <|> ifExpr
   <|> parttern
   <|> doExpr
   <?> "expresion"

term ::Parser Expr
term = try floating
   <|> try int
   <|> try rchar
--   <|> try rstring
   <|> try list
   <|> variable
   <|> parens operator
   <|> parens expr
   <?> "terms" 


variable :: Parser Expr
variable = Var <$> identifier <?> "variable"

operator :: Parser Expr
operator = Var <$> symbolicOp

application :: Parser Expr
application = do 
  var <- variable
  fa <- expr
  return $ Ap var fa

infixApp :: Parser Expr
infixApp = do
  var1 <- term
  op <- operator
  var2 <- term
  return $ Ap (Ap var1 op) var2

lambda :: Parser Expr --lambda expresion
lambda = do
  symbol "\\"
  name <- many identifier
  symbol "."
  expra <- expr
  return $ Lambda name expra

letExpr :: Parser Expr
letExpr = do 
  reservedWords "let"
  expra <- expr
  reservedOp "="
  exprb <- expr
  return $ Let  expra exprb

ifExpr :: Parser Expr
ifExpr = do 
  reservedWords "if"
  expra <- expr
  reservedWords "then"
  exprb <- expr
  reservedWords "else"
  exprc <- expr
  return $ If expra exprb exprc

parttern :: Parser Expr
parttern = do
  reservedWords "case"
  (expra,exprs) <-  L.nonIndented scn (L.indentBlock sc p)
  return $ Parttern expra exprs
  where
    p = do
      body <- term
      reservedOp "of"
      return (L.IndentMany Nothing (return  . (body,)) parttern')

parttern' :: Parser (Expr,Expr)
parttern' = do
  expra <- expr
  reservedOp "=>"
  exprb <- expr
  return (expra,exprb)

doExpr :: Parser Expr
doExpr = do
  reservedWords "do"
  exprs <- many expr
  return $ Do exprs