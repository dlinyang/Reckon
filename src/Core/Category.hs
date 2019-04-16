{-# LANGUAGE GADTs #-}
module Core.Category where

type Name = String

data Cat where
    Object  :: Cat
    Morphism:: Cat -> Cat
    deriving (Ord,Eq,Show)

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
    (×) :: b -> c -> a
    π1  :: a -> b
    π2  :: a -> c

cartesianProduct a b = (a,b)

instance Cartesian Cat where
    (×) = cartesianProduct
    π1 = fst 
    π2 = snd

data CExpr 
    = Var Name Cat
    | Lam [(Name,Cat)] CExpr
    | Let Name Cat CExpr
    | App Name Cat CExpr
    | Comb CExpr CExpr
    deriving (Ord,Eq,Show)

class Deduction a where
    eliminate :: a -> a