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

export
Environment : Type
Environment = List (String, Int -> Int -> Int)

export
envLookup : Environment -> String -> Maybe (Int -> Int -> Int)
envLookup env s = lookup s env

mutual
  show' : MalSexp -> String
  show' MalNil = "nil"
  show' (MalBool True) = "true"
  show' (MalBool False) = "false"
  show' (MalInt i) = show i
  show' (MalSym s) = s
  show' (MalString s) = show s
  show' (MalKeyword s) = s
  show' (MalList xs) = showColl "(" xs ")"
  show' (MalVector xs) = showColl "[" xs "]"
  show' (MalMap xys) = showMap xys

  -- TODO: Combine showColl and showMap?
  -- TODO: How can I make this less verbose but still total?
  showColl : String -> List MalSexp -> String -> String
  showColl start sexps end = showColl' "" sexps where
    showColl' : String -> List MalSexp -> String
    showColl' acc [] = start ++ acc ++ end
    showColl' "" (x :: xs) = showColl' (show' x) xs
    showColl' acc (x :: xs) = showColl' (acc ++ " " ++ show' x) xs

  showMap : List (MalSexp, MalSexp) -> String
  showMap sexps = (showMap' "" sexps) where
    showkv : MalSexp -> MalSexp -> String
    showkv s1 s2 = (show' s1) ++ " " ++ (show' s2)

    showMap' : String -> List (MalSexp, MalSexp) -> String
    showMap' acc [] = "{" ++ acc ++ "}"
    showMap' "" ((x, y) :: xys) = showMap' (showkv x y) xys
    showMap' acc ((x, y) :: xys) = showMap' (acc ++ " " ++ (showkv x y)) xys

export
Show MalSexp where
  show = show'
