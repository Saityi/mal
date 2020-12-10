module main where
open import Data.Unit

open import mio
open import step0_repl

main : IO ⊤
main = repl
