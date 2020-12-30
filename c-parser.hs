import System.Environment
import System.IO
import Data.Char
import Data.List

data AST 
  = Add     AST AST
  | Sub     AST AST
  | Mult    AST AST
  | Div     AST AST
  | Eq      AST AST
  | Gt      AST AST
  | Lt      AST AST
  | Not     AST
  | Assign  AST AST
  | Decl    AST AST
  | Seq     AST AST 
  | Cond    AST AST
  | Loop    AST AST
  | Val     Int
  | Id      String
  | Type    String
  | Empty
  deriving (Show)

types = ["int"]
condToken = ["if"]
loopToken = ["while"]

----- Exp

expr :: String -> (AST,String)
expr xs =
  let (val,rest) = term xs
  in termList val rest

termList val ('+':xs) =
  let (val1,rest) = term xs
  in termList (Add val val1) rest
termList val ('-':xs) =
  let (val1,rest) = term xs
  in termList (Sub val val1) rest
termList val xs = (val,xs)

term :: String -> (AST,String)
term xs =
  let (val,rest) = factor xs
  in factorList val rest

factorList val ('*':xs) =
  let (val1,rest) = factor xs
  in factorList (Mult val val1) rest
factorList val ('/':xs) =
  let (val1,rest) = factor xs
  in factorList (Div val val1) rest
factorList val xs = (val,xs)

factor :: String -> (AST,String)
factor ('(':xs) =
  let (val,rest) = expr xs
  in if succToken rest ')'
    then (val,tail rest)
    else (val,('(':xs))
factor xs = 
  if isDigit $ head xs
    then 
      let tmpNum = span isDigit xs
      in (Val (read (fst tmpNum) :: Int), snd tmpNum)
    else
      variable xs

----- BoolExp

boolExpr :: String -> (AST,String)
boolExpr ('!':xs) =
  let (val,rest) = expr xs
  in (Not val,rest)
boolExpr xs =
  let (val,rest) = expr xs
  in boolList val rest

boolList val ('=':'=':xs) =
  let (val1,rest) = expr xs
  in (Eq val val1, rest)
boolList val ('>':xs) =
  let (val1,rest) = expr xs
  in (Gt val val1, rest)
boolList val ('<':xs) =
  let (val1,rest) = expr xs
  in (Lt val val1,rest)
boolList val xs = expr xs

----- Assign

assign :: String -> (AST,String)
assign xs =
  let (val,rest) = variable xs
  in if succToken rest '='
    then let (val1,rest1) = expr $ tail rest
      in ((Assign val val1), rest1)
    else (val,rest)

variable :: String -> (AST,String)
variable xs =
  let (val,rest) = formatVar xs
  in if validVar val
    then (Id val,rest)
    else (Empty,xs)

formatVar xs = span validChar xs

validVar (x:xs) = x `elem` ['a'..'z'] && and (map validChar xs)
validVar [] = False

validChar x = x `elem` ['A'..'Z'] || x `elem` ['a'..'z'] || x `elem` ['0'..'9']

----- Decl

{-
PROBLEMA:
Catena di dichiarazioni.
Se ho tante dichiarazioni come richiamare la funzione
'declaration' a "catena"?
Termina con ';', si confonde con gli altri statement!

-}

declaration :: String -> (AST,String)
declaration xs =
  let (val,rest) = typeVar xs
      (val1, rest1) = variable rest
  in if succToken rest1 ';'
    then (Decl val val1, tail rest1)
    else if succToken rest1 '='
      then let (val2,rest2) = assign rest
          in (Decl val val2, tail rest2)
      else (Empty,xs)

typeVar :: String -> (AST,String)
typeVar xs =
  let t = filter (checkToken xs) types
  in if t /= []
     then (Type $ head t, drop ((length.head) t) xs)
     else (Empty,xs)

checkToken xs ys = and $ zipWith (==) xs ys

succToken [] _ = False
succToken xs t 
  | head xs == t = True
  | otherwise    = False

----- Cond

condition :: String -> (AST,String)
condition xs =
  let (val,rest) = takeToken (filter (checkToken xs) condToken) xs
  in if succToken rest '('
    then 
      let (val1,rest1) = boolExpr (tail rest)
      in if head rest1 == ')'
        then
          let (val2,rest2) = compose (tail rest1)
          in (Cond val1 val2, rest2)
        else
          (val1,rest1)
    else (Empty,xs)

takeToken [] xs = ("",xs)
takeToken ts xs = (head ts, drop ((length.head) ts) xs)

----- Loop

loop :: String -> (AST,String)
loop xs =
  let (val, rest) = takeToken (filter (checkToken xs) loopToken) xs
  in if succToken rest '('
    then 
      let (val1,rest1) = boolExpr (tail rest)
      in if succToken rest1 ')'
        then
          let (val2,rest2) = compose (tail rest1)
          in (Loop val1 val2, rest2)
        else
          (val1,rest1)
    else (Empty,xs)

----- Block

compose :: String -> (AST,String)
compose ('{':xs) =
  let (val,rest) = stmtChain xs
  in if succToken rest '}'
    then (val,tail rest)
    else (val,rest)
compose xs = (Empty,xs)

----- Stmt

stmtChain :: String -> (AST,String)
stmtChain xs =
  let (val,rest) = stmt xs
  in stmtList val rest

stmtList val (';':'}':xs) =
  (val,'}':xs) 
stmtList val (';':xs) =
  let (val1,rest) = stmt xs
  in stmtList (Seq val val1) rest
stmtList val xs = (val,xs)

stmt :: String -> (AST,String)
stmt xs 
  | checkStmt xs condToken = condition xs
  | checkStmt xs loopToken = loop xs
  | otherwise              = assign xs

checkStmt xs ts 
  | length (filter (checkToken xs) ts) > 0 = True
  | otherwise                              = False

----- Program

program xs =
  let cleanText = foldr (++) [] $ words xs
      (dec,rest) = declaration cleanText
      (prog,rest1) = compose rest
  in (show dec) ++ " " ++ (show prog)

-----

main :: IO ()
main = do
  [inputFile,outputFile] <- getArgs
  file <- readFile inputFile
  let out = show $ compose $ foldr (++) [] $ words file
  putStrLn out
  writeFile outputFile out