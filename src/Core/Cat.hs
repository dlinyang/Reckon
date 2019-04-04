{-# LANGUAGE GADTs #-}
module Core.Cat where

newtype Name = String

data Cat
    = Object Var Domain
    | Morphism Cat Cat
    deriving (Ord,Eq,Show)

data Var = Var Name

data Domain = Domain Name

class MetaCat a where
    id :: a -> a
    compisition :: a -> a -> a

class Cartesian a where
    (<*>) :: b -> c -> a
    p1    :: a -> b
    p2    :: a -> c

data CExpr
    = Lam [Var Domain] CExpr
    | Let Var Domain CExpr
    | App Var Domain Var Domain
    | Comb CExpr CExpr
    deriving (Ord,Eq,Show)