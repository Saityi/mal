module step2_eval

import Printer as P
import Reader as R
import Types

%default total

read : String -> Either String MalSexp
read = R.readString

mutual
  total
  eval_ast : Environment -> MalSexp -> Either String MalSexp
  eval_ast _   (MalList []) =
    MalList <$> pure []
  eval_ast env (MalList (x :: xs)) = assert_total $ -- TODO
    MalList <$> (traverse (eval env) (x :: xs))
  eval_ast _   sexp =
    pure sexp

  %hide apply
  apply : Environment -> MalSexp -> Either String MalSexp
  apply env (MalList [(MalSym sym), (MalInt i), (MalInt j)]) =
    case (envLookup env sym) of
      Just f => Right . MalInt $ f i j
      Nothing => Left $ "Undefined symbol " ++ sym
  apply _ sexp = 
    pure sexp

  eval : Environment -> MalSexp -> Either String MalSexp
  eval env (MalList xs) = (eval_ast env (MalList xs)) >>= (apply env)
  eval _   sexp         = pure sexp

%hide print
print : MalSexp -> Either String String
print = pure . P.prStr

partial
defaultEnv : Environment
defaultEnv =
  [ ("+", (+))
  , ("*", (*))
  , ("-", (-))
  , ("/", (div))]

partial
rep : String -> Either String String
rep s = (read s) >>= (eval defaultEnv) >>= print

%hide repl
partial -- TODO
export
repl : IO ()
repl = do
    putStr "user> "
    eof <- fEOF stdin
    if eof then pure ()
    else do input <- getLine
            case (rep input) of
                Left err => putStrLn err
                Right eval'd => putStrLn eval'd
            repl
 
