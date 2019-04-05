module Reader

import Lightyear.Char
import Lightyear.Combinators
import Lightyear.Core
import Lightyear.Strings

import Types

%default partial

malNil : Parser MalSexp
malNil = token "nil" *> pure MalNil

malBool : Parser MalSexp
malBool = (token "true" *> (pure $ MalBool True)) <|>| 
          (token "false" *> (pure $ MalBool False))

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
readString s = assert_total $ parse malExpr s
-- TODO: Remove assert_total ?
