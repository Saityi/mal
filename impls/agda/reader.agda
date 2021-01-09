module reader where

open import Data.Char using (Char)
open import Data.String using (String) renaming (fromList to 𝕃Char→String; toList to String→𝕃Char)
open import Data.List as 𝕃 using (_∷_; []) renaming (List to 𝕃)
open import Data.List.NonEmpty using () renaming (toList to 𝕃⁺→𝕃; List⁺ to 𝕃⁺)
open import Data.List.Sized.Interface
open import Data.Bool
open import Data.Maybe
open import Data.Nat.Properties
open import Data.Subset
open import Data.Sum
open import Data.Product
open import Function
open import Relation.Unary
open import Relation.Binary.PropositionalEquality.Decidable

open import Induction.Nat.Strong as StrongInd
open import Text.Parser.Types
open import Text.Parser.Combinators
open import Text.Parser.Combinators.Char
open import Text.Parser.Combinators.Numbers
open import Text.Parser.Monad
open import Text.Parser.Position
open Agdarsec′

open import types

instance
   _ = Agdarsec′.monadPlus

𝕃⁺→String : 𝕃⁺ Char → String
𝕃⁺→String = 𝕃Char→String ∘ 𝕃⁺→𝕃

valid-mchars : 𝕃 Char
valid-mchars =
  -- ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-*_=&^%$#@!~`:'/?<>"
  String→𝕃Char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*_=&^%$#@!~`:'/?<>"

skip-mwhitespace : ∀ { P } → ∀[ Parser chars P ⇒ Parser chars P ]
skip-mwhitespace P = mwhitespace ?&> P <&? box mwhitespace
  where mwhitespace = list⁺ (char ',' <|> space)

skip-mcomments : ∀ { P } → ∀[ Parser chars P ⇒ Parser chars P ]
skip-mcomments P = comments ?&> P <&? box comments
  where comments = (char ';' &?> (box $ list⁺ $ anyTokenBut '\n') <&? (box $ char '\n'))

skip-ignorables : ∀ { P } → ∀[ Parser chars P ⇒ Parser chars P ]
skip-ignorables = skip-mwhitespace ∘ skip-mcomments

parse-nil : ∀[ Parser chars s-expression ]
parse-nil = nil <$ text "nil"

parse-bool : ∀[ Parser chars s-expression ]
parse-bool =
  (𝕓 true <$ text "true") <|>
  (𝕓 false <$ text "false")

parse-int : ∀[ Parser chars s-expression ]
parse-int = 𝕚 <$> decimalℤ

parse-sym : ∀[ Parser chars s-expression ]
parse-sym = ‵ ∘ 𝕃⁺→String <$> list⁺ (anyOf valid-mchars)

many-between : ∀ { C S E P } (c : 𝕃 P → C) → ∀[ Parser chars S ⇒ Parser chars E ⇒ □ Parser chars (𝕃⁺ P) ⇒ Parser chars C ]
many-between constr start end parser =
  maybe (constr ∘ 𝕃⁺→𝕃) (constr []) <$>
    (skip-ignorables start &?> parser <& (box $ skip-ignorables end))

pair-parser : ∀ { P } → ∀[ Parser chars P ⇒ Parser chars (cons P P)]
pair-parser p =  (_∙_) <$> p <*> (box p)

parse-string : ∀[ Parser chars s-expression ]
parse-string =
  str ∘ 𝕃Char→String <$>
    (maybe 𝕃⁺→𝕃 []) <$>
      (quotation-mark &?> (box ∘ list⁺ $ anyTokenBut '"') <& (box $ quotation-mark))
  where quotation-mark = char '"' -- workaround for wonky syntax highlighting on Github

mvals : ∀[ Parser chars s-expression ]
mvals = parse-string <|> parse-nil <|> parse-bool <|> parse-int <|> parse-sym

parse-s-expression : ∀[ Parser chars s-expression ]
parse-s-expression = fix (Parser chars s-expression) $ λ rec →
  let □coll-parser  = StrongInd.map list⁺ rec
      vec-parser    = many-between ⟦_⟧ (char '[') (char ']') □coll-parser
      list-parser   = many-between ⦅_⦆ (char '(') (char ')') □coll-parser

      □pairs-parser = StrongInd.map (list⁺ ∘ pair-parser) rec
      map-parser    = many-between (⟅_⟆) (char '{') (char '}') □pairs-parser

  in skip-ignorables (vec-parser <|> list-parser <|> map-parser <|> mvals)

data Either (A : Set) (B : Set) : Set where
  left : A → Either A B
  right : B → Either A B

parse-expr! : String → Either String s-expression
parse-expr! s =
  let parsed = runParser parse-s-expression ≤-refl (Data.String.toVec s) (start , [])
  in result return-input return-input extract-output parsed
  where return-input   = const $ left s
        extract-output = right ∘ Success.value ∘ proj₁
{-# COMPILE GHC parse-expr! as parseExpr #-}
{-# COMPILE GHC Either = data Either (Left | Right) #-}
