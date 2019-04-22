{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Core.Category where

type Name = String

data Cat a where
    Object   ∷ a → Cat a
    Morphism ∷ Cat a → Cat a → Cat a
    deriving (Show)

class MetaCat a where
    id :: a → a
    composition :: a → a → a
    dom:: a → a
    cod:: a → a


class Cartesian a where
    (×) ∷ a → a → (a,a)
    x × y = (x,y)

    π1 ∷ (a,a) → a
    π1 = fst

    π2 ∷ (a,a) → a
    π2  = snd

instance MetaCat (Cat a) where
    id x =  x
    composition (Morphism x y) (Morphism i j) = Morphism x j
    dom (Morphism x y) = x
    cod (Morphism x y) = y

instance Cartesian (Cat a) where
    x × y = (x,y)
    π1 (x,y) = x
    π2 (x,y) = y

type Type = Cat Name

data Λ 
    = Var Name Type
    | Abs [(Name,Type)] Λ
    | Let Name Type Λ
    | App  Λ Λ
    | Comb Λ Λ

class Deduction a where
    introduct ∷ a → a → a
    eliminate ∷ a → a → a
{- 
instance Deduction Λ where
    introduct (Var a b) (Var c d)= Abs [(a,b)] (Var c d)
    eliminate (App a b) (App c d)= App a c -}