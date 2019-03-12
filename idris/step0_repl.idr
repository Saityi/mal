module step0_repl

read : t -> t
read v = v

eval : t -> t
eval v = v

print : t -> t
print v = v

rep : t -> t
rep = print . eval . read

export
repl' : IO ()
repl' = do
    -- Using the builtin 'repl' function doesn't
    -- quite get us the semantics to pass the test
    putStr "user> "
    eof <- fEOF stdin
    if eof then pure ()
    else do input <- getLine
            putStrLn $ rep input
            repl'