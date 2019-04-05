module Types

%default total

public export
data MalSexp : Type where
    MalNil : MalSexp
    MalBool : Bool -> MalSexp
    MalInt : Integer -> MalSexp
    MalSym : String -> MalSexp
    MalString : String -> MalSexp
    MalKeyword : String -> MalSexp
    MalList : (List MalSexp) -> MalSexp
    MalVector : (List MalSexp) -> MalSexp
    MalMap : (List (MalSexp, MalSexp)) -> MalSexp

Environment : Integral a => Type
Environment {a} = List (String, a -> a -> a)

mutual
  showMut : MalSexp -> String
  showMut MalNil = "nil"
  showMut (MalBool True) = "true"
  showMut (MalBool False) = "false"
  showMut (MalInt i) = show i
  showMut (MalSym s) = s
  showMut (MalString s) = show s
  showMut (MalKeyword s) = s
  showMut (MalList xs) = showColl "(" ")" xs
  showMut (MalVector xs) = showColl "[" "]" xs
  showMut (MalMap xys) = showMap xys

  showColl : String -> String -> List MalSexp -> String
  showColl start end sexps = showColl' "" sexps where
    showColl' : String -> List MalSexp -> String
    showColl' acc [] = start ++ acc ++ end
    showColl' acc (x :: xs) = showColl' (acc ++ " " ++ showMut x) xs

  showMap : List (MalSexp, MalSexp) -> String
  showMap sexps = (showMap' "" sexps) where
    showkv : MalSexp -> MalSexp -> String
    showkv s1 s2 = (showMut s1) ++ ", " ++ (showMut s2)

    showMap' : String -> List (MalSexp, MalSexp) -> String
    showMap' acc [] = "{" ++ acc ++ "}"
    showMap' acc ((x, y) :: xys) = showMap' (acc ++ " " ++ (showkv x y)) xys


export
Show MalSexp where
  show = showMut
