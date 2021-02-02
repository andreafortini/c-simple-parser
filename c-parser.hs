import System.Environment
import System.IO
import Data.Char
import Data.List
import Data.List.Split

data AST 
  = Block       [Declaration] [AST] 
  | While       Expr     AST 
  | DoWhile     AST      Expr 
  | If          Expr     AST    AST 
  | Assignment  Variable Expr
  | Error       String
  | Empty
  deriving (Show)

data Expr 
  = Sum   Expr Expr
  | Diff  Expr Expr
  | Mul   Expr Expr
  | Div   Expr Expr
  | Eq    Expr Expr
  | UnEq  Expr Expr
  | Grt   Expr Expr 
  | GrEq  Expr Expr
  | Lest  Expr Expr 
  | LesEq Expr Expr 
  | Not   Expr
  | Neg   Expr
  | Val   Int
  | Var   Variable
  deriving (Show)

data Variable = Id VarName
  deriving (Show)

data Declaration = Decl Variable
  deriving (Show)

type VarName = String

-- Handling errors

instance Eq AST where
  Error _ == Error _ = True
  Error _ == _       = False
  _       == Error _ = False


------------------------
----- Tokens
------------------------

if_token    = "if"
else_token  = "else"
while_token = "while"
do_token    = "do"
for_token   = "for"

------------------------
----- Support functions
------------------------

skipToken :: [Char] -> [Char] -> [Char]
skipToken xs [] = xs
skipToken (x:xs) (t:ts) 
  | x == t    = skipToken xs ts
  | otherwise = (x:xs)

succToken [] _ = False
succToken xs t 
  | head xs == t = True
  | otherwise    = False

checkToken xs ys = and $ zipWith (==) xs ys

formatVar xs = 
  let (var,rest) = span isAlphaNum xs
  in  if validVar var
      then  (var,rest)
      else  ("",xs)

validVar (x:xs) = (isAlpha x) && and (map isAlphaNum xs)
validVar [] = False

--------------------
----- Parser
--------------------

-- expr

expression :: String -> (Expr,String)
expression [] = (Val 1, "")   --Used in for statement for rapresenting infinite loop (missing condition) 
expression xs =
  let (val,rest) = equality xs
  in (val,rest)

equality :: String -> (Expr,String)
equality xs =
  let (val,rest) = comparison xs
  in  comparisonList val rest

comparisonList val ('=':'=':xs) =
  let (val1,rest) = comparison xs
  in comparisonList (Eq val val1) rest
comparisonList val ('!':'=':xs) =
  let (val1,rest) = comparison xs
  in comparisonList (UnEq val val1) rest
comparisonList val xs = (val,xs)

comparison :: String -> (Expr,String)
comparison xs =
  let (val,rest) = term xs
  in termList val rest

termList val ('>':'=':xs) =
  let (val1,rest) = term xs
  in termList (GrEq val val1) rest
termList val ('>':xs) =
  let (val1,rest) = term xs
  in termList (Grt val val1) rest
termList val ('<':'=':xs) =
  let (val1,rest) = term xs
  in termList (LesEq val val1) rest
termList val ('<':xs) =
  let (val1,rest) = term xs
  in termList (Lest val val1) rest
termList val xs = (val,xs)

term :: String -> (Expr,String)
term xs =
  let (val,rest) = factor xs
  in factorList val rest

factorList val ('+':xs) =
  let (val1,rest) = factor xs
  in factorList (Sum val val1) rest
factorList val ('-':xs) =
  let (val1,rest) = factor xs
  in factorList (Diff val val1) rest
factorList val xs = (val,xs)

factor :: String -> (Expr,String)
factor xs =
  let (val,rest) = unary xs
  in unaryList val rest

unaryList val ('*':xs) =
  let (val1,rest) = unary xs
  in unaryList (Mul val val1) rest
unaryList val ('/':xs) =
  let (val1,rest) = unary xs
  in unaryList (Div val val1) rest
unaryList val xs = (val,xs)

unary :: String -> (Expr,String)
unary ('!':xs) =
  let (val,rest) = unary xs
  in (Not val, rest)
unary ('-':xs) =
  let (val,rest) = unary xs
  in (Neg val, rest)
unary xs = primary xs

primary :: String -> (Expr,String)
primary ('(':xs) =
  let (val,rest) = expression xs
  in if succToken rest ')'
    then (val,tail rest)
    else (val,('(':xs))
primary (x:xs)
  | isDigit x = (Val (read (fst tmpNum) :: Int), snd tmpNum)
  | isAlpha x = (Var valVar, rest)
  where tmpNum = span isDigit (x:xs)
        (valVar,rest) = variable (x:xs)

-- variable

variable :: String -> (Variable,String)
variable xs =
  let (val,rest) = formatVar xs
  in (Id val,rest)

-- declaration

declaration :: String -> (Declaration,String)
declaration xs =
  let (val,rest) = variable xs
  in  if succToken rest ';'
      then (Decl val, tail rest)
      else (Decl val, (';':rest))

-- assignment

assignmentStmt xs =
  let (val,rest) = assignment xs
  in  if succToken rest ';'
      then (val, tail rest)
      else (Error "Missing ';'", rest)

assignment :: String -> (AST,String)
assignment xs =
  let (val,rest) = variable xs
  in assignmentSeq val rest

assignmentSeq val ('=':xs) =
  let (val1,rest) = expression xs
  in (Assignment val val1, rest)
assignmentSeq val xs =
  (Error "Missing '='",xs)

-- if statement

ifStmt :: String -> (AST,String)
ifStmt ('(':xs) = 
  let (valCond,rest) = expression xs
  in  if succToken rest ')'
      then ifCond valCond $ tail rest
      else (Error "Missing ')' in if statement", xs)

ifCond valExp xs =
  let (valStmt,rest) = statement xs
  in  if checkToken rest else_token
      then 
        let (valElse,rest1) = optElse $ skipToken rest else_token
        in (If valExp valStmt valElse, rest1)
      else (If valExp valStmt Empty, rest)

optElse xs = statement xs

-- while statement 

whileStmt :: String -> (AST,String)
whileStmt ('(':xs) = 
  let (valCond,rest) = expression xs
  in  if succToken rest ')'
      then whileBlock valCond $ tail rest
      else (Error "Missing ')' in while statement", xs)

whileBlock valCond xs =
  let (valStmt,rest) = statement xs
  in  (While valCond valStmt, rest)

-- do statement

doStmt :: String -> (AST,String)
doStmt ('{':xs) =
  let (valStmt,rest) = statement xs
  in  if succToken rest '}'
      then
        if checkToken (tail rest) while_token
        then 
          let (valCond,rest1) = doWhileCondition $ skipToken (tail rest) while_token
          in  if succToken rest1 ';'
              then (DoWhile valStmt valCond, tail rest1)
              else (Error "Missing ';' after while condition",rest1)
        else (Error "Missing 'while'",rest)
      else (Error "Missing '}' in do statement",xs)
doStmt xs = (Error "Missing '{' in do statement",xs)

doWhileCondition ('(':xs) = 
  let (valCond,rest) = expression xs
  in  if succToken rest ')'
      then (valCond, tail rest)
      else (valCond,(')':xs))

-- for statement

forStmt :: String -> (AST,String)
forStmt ('(':xs) =
  let (parFor,rest) = extractPar xs
  in  if (length parFor) == 3 && succToken rest ')'
      then 
        let (blockFor,rest1) = createBlockFor parFor $ tail rest
        in  (Block [] blockFor,rest1)
      else
        (Error "For syntax", rest)

createBlockFor :: [String] -> String -> ([AST],String)   
createBlockFor (init:cond:step : ps) xs =
  let (valBody, rest)  = createBodyWhile step xs
      (initVal, initR) = assignment init
      (condVal, condR) = expression cond
  in  if initR == "" && condR == ""
      then ( initVal : (While condVal valBody) : [], rest )
      else ([Error "For syntax"], xs)

createBodyWhile :: String -> String -> (AST,String)
createBodyWhile step xs =
  let (val,rest) = statement xs
      (stepVal,_) = assignment step
  in (Block [] ([val] ++ [stepVal]), rest)

extractPar xs = (\ x -> (splitOn ";" $ fst x, snd x) ) $ break (== ')') xs

-- block

compound :: String -> (AST,String)
compound [] = (Empty, "")
compound ('{':xs) =
  let (val,rest) = stmtChain xs
  in if succToken rest '}'
    then (val,tail rest)
    else (Error "Something went wrong :/",rest)

stmtChain :: String -> (AST,String)
stmtChain xs =
  let (decL,rest) = decChain xs
  in  if rest /= []
      then  let (astL,rest1) = astChain rest
            in (Block decL astL, rest1)
      else  (Block decL [], rest)

decChain :: String -> ([Declaration],String)
decChain ('i':'n':'t':xs) =
  let (val,rest) = declaration xs
  in decList [val] rest
decChain xs = ([],xs)

decList val ('i':'n':'t':xs) = 
  let (val1,rest) = declaration xs 
  in decList (val ++ [val1]) rest
decList val xs = (val,xs)

astChain :: String -> ([AST],String)
astChain [] = ([],"")
astChain xs = astList [] xs

astList val [] = (val,"")
astList val ('}':xs) = (val,'}':xs)
astList val xs 
  | val1 == Error "" = ([val1],xs)
  | otherwise = astList (val ++ [val1]) rest
  where (val1,rest) = statement xs

statement xs 
  | checkToken xs if_token    = ifStmt $ skipToken xs if_token
  | checkToken xs while_token = whileStmt $ skipToken xs while_token
  | checkToken xs do_token    = doStmt $ skipToken xs do_token
  | checkToken xs for_token   = forStmt $ skipToken xs for_token
  | isAlpha $ head xs         = assignmentStmt xs
  | head xs == '{'            = compound xs
  | otherwise                 = (Error "Mismatching error",xs)

-- Program

program :: String -> (AST,String)
program xs = compound xs

main :: IO ()
main = do
  [inputFile,outputFile] <- getArgs
  file <- readFile inputFile
  let out = show $ program $ foldr (++) [] $ words file
  putStrLn out
  writeFile outputFile out
