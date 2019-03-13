module step1_read_print
import Reader
import Lightyear.Strings

read : String -> Either String MalSexp
read v = parse malExpr v

eval : t -> t
eval v = v

%hide print
print : Either String MalSexp -> Either String String
print v = map show v

rep : String -> Either String String
rep = print . eval . read

%hide repl
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