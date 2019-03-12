module Reader
import Lightyear.Core
import Lightyear.Char
import Lightyear.Strings
import Lightyear.Combinators

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

Show MalSexp where
    show MalNil = "nil"
    show (MalBool True) = "true"
    show (MalBool False) = "false"
    show (MalInt i) = show i
    show (MalSym s) = s
    show (MalString s) = s
    show (MalKeyword s) = s
    show (MalList sexps) = "(" ++ intercalate ", " (map show sexps) ++ ")"
    show (MalVector sexps)  = "[" ++ intercalate ", " (map show sexps) ++ "]"
    show (MalMap sexps)  = 
        "{" ++ intercalate ", " [show k ++ " " ++ show v | (k, v) <- sexps] ++ "}"

infixl 4 $>
($>) : Functor f => f a -> b -> f b
($>) = flip (map . const)

-- TODO: Whitespace is handled horribly throughout
malNil : Parser MalSexp
malNil = token "nil" $> MalNil

malBool : Parser MalSexp
malBool = token "true" $> MalBool True <|> 
          token "false" $> MalBool False

malInt : Parser MalSexp
malInt = MalInt <$> integer

commas : Parser ()
commas = skip (many (char ','))

malWhitespace : Parser Char
malWhitespace = char ',' <|> space

mutual 
    malValue : Parser MalSexp
    malValue = (malNil <?> "nil")
           <|>| (malBool <?> "bool")
           <|>| (malInt  <?> "int")
           <|>| (malList <?> "list")
           <|>| (malVec  <?> "vec")
           <|>| (malMap  <?> "map")

    malColl : (t -> MalSexp) -> Char -> Parser t -> Char -> Parser MalSexp
    malColl ctor start itemsParser end = do
        char start
        spaces
        items <- itemsParser
        spaces
        char end
        pure $ ctor items

    malList : Parser MalSexp
    malList = malColl MalList '(' (malValue `sepBy` malWhitespace) ')'

    malVec : Parser MalSexp
    malVec = malColl MalVector '[' (malValue `sepBy` malWhitespace) ']'

    malMap : Parser MalSexp
    malMap = malColl MalMap '{' (kvParser `sepBy` malWhitespace) '}'
        where kvParser : Parser (MalSexp, MalSexp)
              kvParser = do
                key <- malValue
                space
                value <- malValue
                pure $ (key, value)

export
test : IO ()
test = case parse malValue "[1 2 {4 5 6 7}]" of
    Left err => putStrLn err
    Right q  => putStrLn $ show q
