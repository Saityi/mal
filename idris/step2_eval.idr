module step2_eval

import Printer as P
import Reader as R
import Types

%default total

read : String -> Either String MalSexp
read = R.readString

mutual
  eval_ast : Environment -> MalSexp -> Either String MalSexp
  eval_ast env (MalList []) = pure $ MalList []
  eval_ast env (MalList (x :: xs)) = case (eval env x) of
    Right x' => assert_total $ case (eval_ast env (MalList xs)) of
      Right (MalList xs') => pure . MalList $ x' :: xs'
      Right sexp          => Left $ "wat"
      Left err            => Left err
    Left err => Left err
  eval_ast env sexp = pure sexp

  eval : Environment -> MalSexp -> Either String MalSexp
  eval env (MalList []) = pure (MalList [])
  eval env (MalList (x :: xs)) = assert_total $ case (eval_ast env (MalList (x :: xs))) of
    Right (MalList ((MalSym sym) :: (MalInt i) :: (MalInt j) :: Nil)) => case (envLookup env sym) of
      Just f => pure . MalInt $ f i j
      Nothing => Left $ "Failed to look up symbol " ++ sym ++ " in environment."
    Right (MalList ((MalSym sym) :: xs)) => Left $ "Wrong number of args for function " ++ sym
    Right sexp => pure sexp
    Left err => Left err
  eval env sexp = pure sexp

%hide print
print : MalSexp -> Either String String
print = pure . P.prStr

partial
defaultEnv : Environment
defaultEnv = [ ("+", (+))
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
 
