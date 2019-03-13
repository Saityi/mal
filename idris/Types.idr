module Types

public export
data MalSexp = MalNil
             | MalBool Bool
             | MalInt Integer
             | MalSym String
             | MalString String
             | MalKeyword String
             | MalList (List MalSexp)
             | MalVector (List MalSexp)
             | MalMap (List (MalSexp, MalSexp))

intercalate : String -> List String -> String
intercalate sep [] = ""
intercalate sep [s] = s
intercalate sep (s :: ss) = s ++ sep ++ (intercalate sep ss)

export
Show MalSexp where
    show MalNil = "nil"
    show (MalBool True) = "true"
    show (MalBool False) = "false"
    show (MalInt i) = show i
    show (MalSym s) = s
    show (MalString s) = show s
    show (MalKeyword s) = s
    show (MalList sexps) = assert_total $ "(" ++ unwords (map show sexps) ++ ")"
    show (MalVector sexps)  = assert_total $ "[" ++ unwords (map show sexps) ++ "]"
    show (MalMap sexps)  = assert_total $
        "{" ++ intercalate ", " [show k ++ " " ++ show v | (k, v) <- sexps] ++ "}"