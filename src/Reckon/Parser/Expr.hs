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
term = try literal
   <|> list
   <|> set
   <|> typeNotation
   <|> variable
   <|> try (parens operator)
   <|> parens expr
   <?> "terms" 

list :: Parser Expr

set :: Parser Expr

typeNotation :: Parser Expr
 
variable :: Parser Expr
variable = Var <$> identifier <?> "variable"

operator :: Parser Expr
operator = Var <$> symbolicOp

prefixFun :: Parser Expr
prefixFun = variable
    <|> parens operator

--reservedPrefixFun ::  String -> Parser Expr
--reservedPrefixFun = parens reservedOp

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
    curry'  =  foldl Ap 

infixApp :: Parser Expr
infixApp = makeExprParser term operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =[[infixlOp Times "*"
                ,infixlOp Divide "/"]
               ,[infixlOp Plus "+"
                ,infixlOp Minus "-"]
               ,[InfixL (infixFun' <$> infixFun)]
               --,[Infix]
                ]

--prime function operators to operators table

infixlOp opn opc  = InfixL (Op <$> primeFun opn opc )

primeFun opname opchar= opname <$ reservedOp opchar

-- symbilic operators to operators table
infixFun' op x  = Ap (Ap op x) 

-- , oparetor to operators table


lambda :: Parser Expr --lambda expresion
lambda = do
  reservedOp "\\" <|> reservedOp "λ"
  name <- many identifier
  symbol "."
  Lambda name <$> expr

letExpr :: Parser Expr
letExpr = do 
  reservedWords "let"
  bind <- expr
  reservedOp "="
  Let  bind <$> expr

ifExpr :: Parser Expr
ifExpr = do 
  reservedWords "if"
  condition <- expr
  reservedWords "then"
  ture <- expr
  reservedWords "else"
  If condition ture <$> expr

caseExpr :: Parser Expr --bugs:multy indent 
caseExpr = do
  (var,exprs) <- L.nonIndented scn (L.indentBlock scn p)
  return $ Case var exprs
  where
    p = do
        reservedWords "case"
        var <- term
        reservedWords "of"
        return (L.IndentSome Nothing (return . (var,)) caseExprBody)

caseExprBody :: Parser (Expr,Expr)
caseExprBody = do
  expra <- expr
  reservedOp "=>" <|> reservedOp "⇒"
  exprb <- expr
  return (expra,exprb)

doExpr :: Parser Expr
doExpr =do
  reservedWords "do"
--  l <- lookAheadMath  (symbol "(")
  exprs <- L.nonIndented scn (L.indentBlock scn body)
  return $ Do exprs
  where 
    body = return $ L.IndentSome Nothing return expr