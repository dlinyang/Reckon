{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE TupleSections #-}
module Reckon.Parser.Decl where

import Reckon.Syntax
import Reckon.Lexer
import Reckon.Parser.Expr
import Text.Megaparsec 
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

declare :: Parser Decl --parse declare
declare = try funDecl
      <|> try opDecl 
      <|> try typeDecl 
      <|> modDecl
      <?> "declare"

funDecl :: Parser Decl
funDecl = do
  name <- identifier
  args  <- many term
  reservedOp "="
  FunDecl (Var name) args <$> expr

opDecl :: Parser Decl
opDecl = do
  op <- parens operator
  args <- many term
  reservedOp "="
  FunDecl op args <$> expr

typeDecl :: Parser Decl --bug:wait fix
typeDecl = do
  name <- identifier
  reservedOp ":"
  TypeDecl name <$> rtype

rtype = makeExprParser rtypeTerm [[InfixL (TT <$ reservedOp "->")]]

rtypeTerm = rtypec
     <|> parens rtype

rtypec = TC <$> identifier

modDecl :: Parser Decl
modDecl = modDef
      <|> modImp
      <|> modExp

modDef :: Parser Decl
modDef = do
  reservedWords "module"
  ModuleDecl . MDef <$> identifier

modImp :: Parser Decl
modImp = do
  reservedWords "import"
  ModuleDecl . MImp <$> identifier

modExp :: Parser Decl
modExp = do
  reservedWords "export"
  names <- parens (many identifier)
  return $ ModuleDecl (MExp names)