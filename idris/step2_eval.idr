module step2_eval

import Printer as P
import Reader as R
import Types

%default total

read : String -> Either String MalSexp
read = R.readString

partial
eval : Environment -> MalSexp -> Either String MalSexp
eval env (MalList ((MalSym s) :: args)) = case (envLookup env s) of
  Just sym => case args of
    (MalInt i :: MalInt j :: []) => Right . MalInt $ (sym i j)
    (s1 :: s2 :: []) =>
      case (eval env s1) of
        Right (MalInt i) =>
          case (eval env s2) of
            Right (MalInt j) => Right . MalInt $ sym i j
            Right (sexp) => Left $ "Invalid arg " ++ (show sexp) ++ " to " ++ s
            err => err
        Right (sexp) => Left $ "Invalid arg " ++ (show sexp) ++ " to " ++ s
        err => err
    _ => Left "Invalid number of args."
  Nothing  => Left ("Failed to lookup symbol " ++ s ++ " in environment.")
eval env sexp = Right sexp

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
 
