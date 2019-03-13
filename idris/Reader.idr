module Reader
import Lightyear.Core
import Lightyear.Char
import Lightyear.Strings
import Lightyear.Combinators
import Types

infixl 4 $>

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
    malExpr : Parser MalSexp
    malExpr = (opt . many $ malWhitespace) *> malExprI <* (opt . many $ malWhitespace)

    malExprI : Parser MalSexp
    malExprI = (opt $ malComment) *> (malNil <?> "nil")
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
readString : String -> Either String MalSexp
readString s = parse malExpr s