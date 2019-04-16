{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE TupleSections #-}
module Reckon.Parser.Expr where

import Reckon.Syntax
import Reckon.Lexer
import Reckon.Parser.Data
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr


expr :: Parser Expr
expr = try application
   <|> term
   <|> lambda
   <|> letExpr
   <|> ifExpr
   <|> caseExpr
   <|> doExpr
   <?> "expresion"

term ::Parser Expr
term = try floating
   <|> try int
   <|> try rchar
   <|> try rstring
   <|> try list
   <|> variable
   <|> try (parens operator)
   <|> parens expr
   <?> "terms" 


variable :: Parser Expr
variable = Var <$> identifier <?> "variable"

operator :: Parser Expr
operator = Var <$> symbolicOp

prefixFun :: Parser Expr
prefixFun = variable
    <|> parens operator

infixFun :: Parser Expr
infixFun = operator
       <|> accent variable 

application :: Parser Expr
application = try prefixApp
          <|> try infixApp

prefixApp :: Parser Expr
prefixApp = do 
  fun <- prefixFun
  args <- some term -- at least one
  return $ curry fun args
  where 
    curry fun (arg:args) = curry' (Ap fun arg) args
    curry' ap [] = ap
    curry' ap (arg:args) = curry' (Ap ap arg) args

infixApp :: Parser Expr
infixApp = makeExprParser term operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =[map infixlOp opll ++ [InfixL (infixFun' <$> infixFun)]]

--prime function operators to operators table
opll = [(Plus,"+"),(Minus,"-"),(Times,"*"),(Divide,"/"),(Mod,"%")] -- left associated operators list

infixlOp (opn,opc)  = InfixL (binary opn opc)

binary opn opc = Op <$> primeFun opn opc 

primeFun opname opchar= opname <$ reservedOp opchar

-- symbilic operators to operators table
infixFun' op x  = Ap (Ap op x) 

-- , oparetor to operators table


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

caseExpr :: Parser Expr --bugs:multy indent 
caseExpr = do
  (var,exprs) <- L.nonIndented scn (L.indentBlock scn p)
  return $ Case var exprs
  where
    p = do
        reservedWords "case"
        var <- term
        reservedWords "of"
        return (L.IndentSome Nothing (return . (var,)) caseExpr')

caseExpr' :: Parser (Expr,Expr)
caseExpr' = do
  expra <- expr
  reservedOp "=>"
  exprb <- expr
  return (expra,exprb)

doExpr :: Parser Expr
doExpr =do
  reservedWords "do"
  exprs <- many (parens expr)
  return $ Do exprs