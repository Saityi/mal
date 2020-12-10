module mio where

open import Agda.Builtin.IO using (IO) public
open import Function
open import Data.Unit
open import Data.String

infixl 1 _>>=_

postulate
  return : ∀ {A : Set} → A → IO A
  _>>=_  : ∀ {A B : Set} → IO A → (A → IO B) → IO B
  _<$>_  : ∀ {A B : Set} → (A → B) → IO A → IO B
  _⊛_    : ∀ {A B : Set} → IO (A → B) → IO A → IO B

  putStr : String -> IO ⊤
  putStrLn : String → IO ⊤
  getLine : IO String

  Handle : Set
  hFlush : Handle → IO ⊤
  stdout : Handle

{-# FOREIGN GHC import qualified System.IO #-}
{-# FOREIGN GHC import qualified Data.Text.IO #-}

{-# COMPILE GHC return = \ _ -> return #-}
{-# COMPILE GHC _>>=_ = \ _ _ -> (>>=) #-}
{-# COMPILE GHC _<$>_ = \ _ _ -> (<$>) #-}
{-# COMPILE GHC _⊛_ = \ _ _ -> (<*>) #-}

{-# COMPILE GHC putStr = Data.Text.IO.putStr #-}
{-# COMPILE GHC getLine = Data.Text.IO.getLine #-}
{-# COMPILE GHC putStrLn = Data.Text.IO.putStrLn #-}

{-# COMPILE GHC Handle = type System.IO.Handle #-}
{-# COMPILE GHC hFlush = System.IO.hFlush #-}
{-# COMPILE GHC stdout = System.IO.stdout #-}

infixl 1 _>>_
_>>_ : ∀ {A B : Set} → IO A → IO B → IO B
effa >> effb = effa >>= λ _ → effb

infixl 4 _$>_
_$>_ : ∀ {A B : Set} → IO A → B → IO B
effa $> b = const b <$> effa

infixl 1 _<<_
_<<_ : ∀ {A B : Set} → IO B → IO A → IO B
_<<_ = flip _>>_

infixl 4 _<$_
_<$_ : ∀ {A B : Set} → B → IO A → IO B
_<$_ = flip _$>_

infixl 1 _<&>_
_<&>_ : ∀ {A B : Set} → IO A → (A → B) → IO B
_<&>_ = flip _<$>_
