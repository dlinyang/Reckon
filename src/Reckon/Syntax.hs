{-# LANGUAGE GADTs #-}
module Reckon.Syntax where

type Name = String
-- name -> identifier

data Stmt
    = D Decl
    | E Expr
    deriving (Eq,Ord,Show)

data Expr 
    = Var Name --bound variable
    | Literal Lit --element
    | List [Expr]
    | Set [Expr]
    | TypeNotation Type
    | Op PrimeFun Expr Expr -- axiom
    | Ap Expr Expr --application
    | Lambda [Name] Expr -- lambda expression
    | Let Expr Expr -- variable binding
    | If Expr  Expr  Expr
    | Case Expr [(Expr,Expr)]
    | Do [Expr] 
    deriving (Eq,Ord,Show)

data Lit
    = RInteger Integer
    | RFloat Float
    | RChar Char
    | RString String
    deriving (Eq,Ord,Show)

data PrimeFun
    = Cons |App | Head | Tail -- list operator
    | Plus | Minus | Times | Divide | Mod -- 
    | RAnd | ROr   |REq    | RNeq -- 
    deriving (Eq,Ord,Show)

data Decl 
    = FunDecl Expr [Expr] Expr -- Variable:Function Variable:Parameter = body
    | TypeDecl Name Type
    | TypeConsDecl Name Cons
    | ModuleDecl Modecl  
    deriving (Eq,Ord,Show)

data Type 
    = TC Name
    | TT Type Type
    deriving (Eq,Ord,Show)

data Cons
    = N Name [Type]
    | GADT Decl
    deriving (Eq,Ord,Show)

data Modecl
    = ModuleDefine Name
    | ModuleImport Name
    | ModuleExport [Name]
    deriving (Eq,Ord,Show)