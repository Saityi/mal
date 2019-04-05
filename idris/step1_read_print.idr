module step1_read_print

import Printer as P
import Reader as R
import Types

%default total

read : String -> Either String MalSexp
read = R.readString

eval : t -> t
eval v = v

%hide print
print : Either String MalSexp -> Either String String
print = map P.prStr

rep : String -> Either String String
rep = print . eval . read

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
