module Reader
import Lightyear.Core
import Lightyear.Char
import Lightyear.Strings
import Lightyear.Combinators

export
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
    show (MalList sexps) = "(" ++ unwords (map show sexps) ++ ")"
    show (MalVector sexps)  = "[" ++ unwords (map show sexps) ++ "]"
    show (MalMap sexps)  = 
        "{" ++ intercalate ", " [show k ++ " " ++ show v | (k, v) <- sexps] ++ "}"

infixl 4 $>
($>) : Functor f => f a -> b -> f b
($>) = flip (map . const)

-- TODO: Whitespace is handled horribly throughout
malNil : Parser MalSexp
malNil = token "nil" $> MalNil

malBool : Parser MalSexp
malBool = token "true" $> MalBool True <|>| 
          token "false" $> MalBool False

malInt : Parser MalSexp
malInt = MalInt <$> integer

malWhitespace : Parser Char
malWhitespace = char ',' <|>| space

validChars : List Char
validChars = (with List (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ (unpack "+-*_=&^%$#@!~`:'/?<>")))

malSym : Parser MalSexp
malSym = MalSym . pack <$> some (satisfy (\c => c `elem` validChars))

malString : Parser MalSexp
malString = MalString . pack <$> between (char '"') (char '"') (many (satisfy (\c => c `elem` (validChars ++ (unpack " (){}[].,\\|;")))))

malComment : Parser ()
malComment = char ';' *> (skip . many $ anyChar) <* endOfLine

mutual
    export
    malExpr : Parser MalSexp
    malExpr = (opt . many $ malWhitespace) *> malExpr' <* (opt . many $ malWhitespace)

    malExpr' : Parser MalSexp
    malExpr' = (skip . opt $ malComment) *> (malNil <?> "nil")
           <|>| (malBool <?> "bool")
           <|>| (malSym <?> "symbol")
           <|>| (malString <?> "string")
           <|>| (malInt  <?> "int")
           <|>| (malList <?> "list")
           <|>| (malVec  <?> "vec")
           <|>| (malMap  <?> "map")

    malList : Parser MalSexp
    malList = MalList <$> between (char '(') (char ')') (many malExpr)

    malVec : Parser MalSexp
    malVec = MalVector <$> between (char '[') (char ']') (many malExpr)

    malMap : Parser MalSexp
    malMap = MalMap <$> between (char '{') (char '}') (many kvParser)
        where kvParser : Parser (MalSexp, MalSexp)
              kvParser = do
                key <- malExpr
                skip . opt $ many malWhitespace
                value <- malExpr
                pure $ (key, value)

export
test : IO ()
test = case parse malExpr "[1 2 {4 5 6 7}]" of
    Left err => putStrLn err
    Right q  => print $ show q
