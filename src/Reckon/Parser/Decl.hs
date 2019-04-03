{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE TupleSections #-}
module Reckon.Parser.Decl where

import Reckon.Syntax
import Reckon.Lexer
import Reckon.Parser.Expr
import Text.Megaparsec 
import qualified Text.Megaparsec.Char.Lexer as L

declare :: Parser Decl --parse declare
declare = try funDecl
      <|> try opDecl 
      <|> try typeDecl 
      <|> modDecl
      <?> "declare"

funDecl :: Parser Decl
funDecl = do
  name <- identifier
  arg  <- many term
  reservedOp "="
  body <- expr 
  return $ FunDecl (Var name) arg body

opDecl :: Parser Decl
opDecl = do
  name <- parens operator
  arg <- many term
  reservedOp "="
  body <- expr
  return $ FunDecl (name) arg body

typeDecl :: Parser Decl --bug:wait fix
typeDecl = do
  name <- identifier
  reservedOp ":"
  ty <- rtype
  return $ TypeDecl name ty

rtype = parens rtype
    <|> rtypec
    <|> rtypet
    <?> "type"

rtypec = TC <$> identifier

rtypet = do
  t1 <- rtypec
  reservedOp "->"
  t2 <- rtypec
  return $ TT t1 t2

modDecl :: Parser Decl
modDecl = modDef
      <|> modImp

modDef :: Parser Decl
modDef = do
  reservedWords "module"
  name <- identifier
  reservedWords "where"
  return $ ModuleDecl (MDef name)

modImp :: Parser Decl
modImp = do
  reservedWords "import"
  name <- identifier
  return $ ModuleDecl (MImp name)