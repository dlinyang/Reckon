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
    | Type Type
    | Op PrimeFun Expr Expr -- axiom
    | Ap Expr Expr --application
    | Lambda [Name] Expr -- lambda expression
    | Let Expr Expr -- variable binding
    | If Expr  Expr  Expr
    | Parttern Expr [(Expr,Expr)]
    | Do [Expr] 
    deriving (Eq,Ord,Show)

data Lit
    = RInteger Integer
    | RFloat Float
    | RChar Char
    | RString String
    | List [Lit]
    deriving (Eq,Ord,Show)


data PrimeFun
    = Cons |App | Head | Tail -- list operator
    | Def | Ty -- (=) :
    | Plus | Minus | Times | Divide -- 
    | RAnd | ROr | RNot |REq -- 
    deriving (Eq,Ord,Show)

data Decl 
    = FunDecl Expr [Expr] Expr -- Variable:Function Variable:Parameter = body
    | TypeDecl Name Type
    | TypeConsDecl Type Cons
    | ModuleDecl Modecl  
    deriving (Eq,Ord,Show)

data Type 
    = TC Name
    | TT Type Type
    | TN [Type]
    deriving (Eq,Ord,Show)

data Cons
    = N Name 
    | GADT Decl
    deriving (Eq,Ord,Show)

data Modecl
    = MDef Name
    | MImp Name
    deriving (Eq,Ord,Show)