{-# LANGUAGE GADTs #-}
module Core.Category where

type Name = String

data Cat a where
    Object  :: Cat a
    Morphism:: Cat a -> Cat a
    deriving (Ord,Eq,Show)

type Type = Cat Name

class MetaCat a where
    id :: a -> a
    composition :: a -> a -> a
    dom:: a -> a
    cod:: a-> a

instance MetaCat Cat a where
    id a =  a
    composition (Morphism x y) (Morphism y z)  = (Morphism x z)
    dom Morphism x y = x
    cod Morphism x y = y

class Cartesian a where
    (×) :: b -> c -> a
    π1  :: a -> b
    π2  :: a -> c

instance Cartesian Cat a where
    (×) （Cat a) (Cat b) = (Cat a,Cat b)
    π1 (Cat a,Cat b)     = Cat a  
    π2 (Cat a,Cat b)     = Cat b

data Λ 
    = Var Name Type
    | Lam [(Name,Type)] Λ
    | Let Name Type Λ
    | App  Λ Λ
    | Comb Λ Λ
    deriving (Ord,Eq,Show)

class Deduction a where
    introduct :: a -> a -> a
    eliminate :: a -> a -> a

instance Deduction Λ where
    introduct (Var a b) (Var c d)= Lam [(Var a b)] (Var c d)
    eliminate (App a b) (App b c)= App a c
    eliminate  ()