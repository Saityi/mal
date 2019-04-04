module Reader

import public Text.Lexer
import public Text.Parser
import Types

%default total

infixl 4 $>
($>) : Functor f => f a -> b -> f b
($>) = flip (map . const)

validChars : List Char
validChars = (with List (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ (unpack "+-*_=&^%$#@!~`:'/?<>")))

validIdent : Char -> Bool
validIdent = (`elem` validChars)

data MalToks = NilTok
             | IntTok String
             | Identifier String
             | Comment String
             | StrLit String
             | CharLit String
             | LParen
             | RParen
             | LBrack
             | RBrack
             | LBrace
             | RBrace
             | EOF

Show MalToks where
  show NilTok         = "Nil"
  show (IntTok x)     = "Int " ++ x
  show (Identifier x) = "Identifier " ++ x
  show (Comment _)    = ""
  show (StrLit x)     = "StrLit " ++ x
  show (CharLit x)    = "CharLit " ++ x
  show LParen         = "LParen"
  show RParen         = "RParen"
  show LBrack         = "LBrack"
  show RBrack         = "RBrack"
  show LBrace         = "LBrace"
  show RBrace         = "RBrace"
  show EOF            = ""

Eq MalToks where
   (==) (IntTok x) (IntTok y)         = x == y
   (==) (Identifier x) (Identifier y) = x == y
   (==) (Comment x) (Comment y)       = x == y
   (==) (StrLit x) (StrLit y)         = x == y
   (==) (CharLit x) (CharLit y)       = x == y
   (==) NilTok NilTok                 = True
   (==) LParen LParen                 = True
   (==) RParen RParen                 = True
   (==) LBrack LBrack                 = True
   (==) RBrack RBrack                 = True
   (==) LBrace LBrace                 = True
   (==) RBrace RBrace                 = True
   (==) EOF EOF                       = True
   (==) _ _                           = False

malComment : Lexer
malComment = lineComment (is ';')

identifier : Lexer
identifier = pred validIdent <+> many (pred validIdent)

tokenMap : TokenMap MalToks
tokenMap =
  [ (malComment, Comment)
  , (is ' ', Comment)
  , (is ',', Comment)
  , (is '\n', Comment)
  , (exact "nil", const NilTok)
  , (is '(', const LParen)
  , (is '[', const LBrack)
  , (is '{', const LBrace)
  , (is '}', const RBrace)
  , (is ']', const RBrack)
  , (is ')', const RParen)
  , (intLit, IntTok)
  , (charLit, CharLit)
  , (stringLit, StrLit)
  , (identifier, Identifier)]

notComment : MalToks -> Bool
notComment (Comment _) = False
notComment _           = True

malTokens : String -> Either String (List MalToks)
malTokens s = let (toks, line, col, rem) = lex tokenMap s in
  if (length rem == 0)
  then Right . filter notComment . map tok $ toks
  else Left $ "Invalid input on line " ++ (show line) ++ ", col " ++ (show col)

Parser : Type -> Type
Parser t = Grammar MalToks True t

exactMatch : MalToks -> (MalToks -> MalSexp) -> Parser MalSexp
exactMatch t ctor = terminal $ (\tok =>
  if (t == tok) then Just (ctor tok) else Nothing)

malNil : Parser MalSexp
malNil = exactMatch NilTok (const MalNil)
