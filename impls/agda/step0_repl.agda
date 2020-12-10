module step0_repl where

open import Data.String
open import Data.Unit
open import Function

open import mio

prompt : IO ⊤
prompt = putStr "user> " >> hFlush stdout

read : IO String
read = prompt >> getLine

eval : ∀ {a : Set} → a → IO a
eval = return

print : String → IO ⊤
print = putStrLn

{-# NON_TERMINATING #-}
repl : IO ⊤
repl = read  >>= eval >>= print >> repl

