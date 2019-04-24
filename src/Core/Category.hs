{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}

module Core.Category where

type Name = String

data 𝒞 a where -- category
    Object   ∷ a → 𝒞 a -- Obj𝒞 
    Morphism ∷ 𝒞 a → 𝒞 a → 𝒞 a -- A → B 
    deriving (Show)

type Cat a = 𝒞 a

class MetaCat a where
    id :: a → a
    composition :: a → a → a
    dom:: a → a
    cod:: a → a

class Cartesian a where
    (×) ∷ a → a → (a,a)
    x × y = (x,y)

    π₁ ∷ (a,a) → a
    π₁ = fst

    π₂ ∷ (a,a) → a
    π₂  = snd

instance MetaCat (𝒞 a) where
    id x =  x
    composition (Morphism x y) (Morphism i j) = Morphism x j
    dom (Morphism x y) = x
    cod (Morphism x y) = y

instance Cartesian (𝒞 a) where
    x × y = (x,y)
    π₁ (x,y) = x
    π₂ (x,y) = y

type Type = 𝒞 Name

data Λ 
    = Var Name Type
    | Abs [(Name,Type)] Λ
    | Let Name Type Λ
    | App  Λ Λ
    | Comb Λ Λ

class Deduction a where
    introduction ∷ a → a → a
    elimination  ∷ a → a → a

instance Deduction Λ where
    introduction (Var a b) (Var c d)= Abs [(a,b)] (Var c d)
    introduction a b = Comb a b
    elimination (App a b) (App c d)= App a c 
    elimination a b = Comb a b

class  Relation a b c where
    reflextive :: (a -> a) -> (a -> a)
    transitive :: (a -> b) -> (b -> c) -> c
    symetric   :: (a -> b) -> (b -> a)