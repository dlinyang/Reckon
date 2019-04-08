{-# LANGUAGE GADTs #-}
module Core.Category where

newtype Name = String

data Cat where
    Object::Name
    Morphism::Cat -> Cat
    deriving (Ord,Eq,Show,MetaCat,Cartesian)

class MetaCat a where
    id :: a -> a
    composition :: a -> a -> a
    dom:: a -> a
    cod:: a-> a

instance MetaCat Cat where
    id a =  a
    composition f g h = (f g) h
    dom Morphism x y = x
    cod Morphism x y = y

class Cartesian a where
    (><) :: b -> c -> a
    p1   :: a -> b
    p2   :: a -> c

cartesianProduct a b = (a,b)

instance Cartesian Cat where
    (><) = cartesianProduct
    p1 = fst 
    p2 = snd

data CExpr 
    = Var Name Cat
    | Lam [Name Cat] CExpr
    | Let Name Cat CExpr
    | App Name Cat CExpr
    | Comb CExpr CExpr
    deriving (Ord,Eq,Show,Deduction)

class Deduction a where
    eliminate :: a -> a