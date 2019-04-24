{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
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
      <|> moduleDecl
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

rtype = makeExprParser rtypeTerm [[InfixL (TT <$ (reservedOp "->" <|> reservedOp "→"))]]

rtypeTerm = rtypec
        <|> parens rtype

rtypec = TC <$> identifier

moduleDecl :: Parser Decl
moduleDecl = moduleDefine
         <|> moduleImport
         <|> moduleExport

moduleDefine :: Parser Decl
moduleDefine = do
  reservedWords "module"
  ModuleDecl . ModuleDefine <$> identifier

moduleImport :: Parser Decl
moduleImport = do
  reservedWords "import"
  ModuleDecl . ModuleImport <$> identifier

moduleExport :: Parser Decl
moduleExport = do
  reservedWords "export"
  names <- parens (many identifier)
  return $ ModuleDecl (ModuleExport names)