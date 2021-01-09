module types where

open import Category.Functor
open import Category.Applicative
open import Category.Monad
open import Size
open import Function
open import Data.Bool using () renaming (Bool to 𝔹)
open import Data.Bool.Show using () renaming (show to 𝔹show)
open import Data.Integer using (ℤ) renaming (show to ℤshow)
open import Data.Nat as ℕ using (ℕ)
open import Data.Sum
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary
open import Relation.Nullary.Decidable
open import Data.String as 𝕊 using (String; _++_) renaming (show to strShow; concat to strConcat)
open import Data.List as List using (_∷_; []; List)
open import Data.Unit
open import Data.Maybe
open import Data.Maybe.Categorical
open import Data.String.Properties using () renaming (<-strictTotalOrder-≈ to stringOrder)
open import Data.Integer hiding (show)
open import Data.Integer.DivMod
open import Data.Integer.Properties
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary
open import Relation.Nullary.Decidable
open import Function
open import Data.Nat using (suc; _≟_)
open import Data.Vec

{-# FOREIGN GHC data Cons a b = Cons { car :: a, cdr :: b } deriving Show #-}
infix 9 _∙_
record cons (A B : Set) : Set where
  constructor _∙_
  field
    car : A
    cdr : B
{-# COMPILE GHC cons = data Cons ( Cons ) #-}

import Data.AVL.Map stringOrder as M

variable
  N : Set

{-# FOREIGN GHC data SExpr = SNil | SBool Bool | SInt Integer | SSymbol Data.Text.Text | SString Data.Text.Text | SKeyword Data.Text.Text | SList [SExpr] | SVector [SExpr] | SMap [Cons SExpr SExpr] deriving Show #-}
{-# FOREIGN GHC type SExpra a = SExpr #-}
data s-expression {i : Size} : Set where
  nil : s-expression
  𝕓   : 𝔹 → s-expression
  𝕚   : ℤ → s-expression
  ‵   : String → s-expression
  str : String → s-expression
  ⦂   : String → s-expression

  ⦅_⦆ : ∀ {j : Size< i}
      → List (s-expression {j})
      → s-expression

  ⟦_⟧ : ∀ {j : Size< i}
      → List (s-expression {j})
      → s-expression

  ⟅_⟆ : ∀ {j k : Size< i}
      → List (cons (s-expression {j})
                   (s-expression {k}))
      → s-expression
{-# COMPILE GHC s-expression = data SExpra ( SNil | SBool | SInt | SSymbol | SString | SKeyword | SList | SVector | SMap ) #-}

show : ∀ {i} (_ : s-expression {i}) → String
show-coll : ∀ {i} → List (s-expression {i}) → String → String → String
show-coll xs start end = start 𝕊.++ (𝕊.concat ∘ List.intersperse " " ∘ List.map show $ xs) 𝕊.++ end

show nil          = "nil"
show (𝕓 b)        = 𝔹show b
show (𝕚 i)        = ℤshow i
show (‵ s)        = s
show (str s)      = strShow s
show (⦂ k)        = k
show ⦅ [] ⦆       = "()"
show ⟦ [] ⟧       = "[]"
show ⟅ [] ⟆       = "{}"
show ⦅ x ∷ xs ⦆   = show-coll (x ∷ xs) "(" ")"
show ⟦ x ∷ xs ⟧   =  show-coll (x ∷ xs) "[" "]"
show ⟅ kv ∷ kvs ⟆ = "{" 𝕊.++ (𝕊.concat $ List.map show-cons (kv ∷ kvs)) 𝕊.++ "}"
  where
    show-cons : cons s-expression s-expression → String
    show-cons (k ∙ v) = show k 𝕊.++ " " 𝕊.++ show v

