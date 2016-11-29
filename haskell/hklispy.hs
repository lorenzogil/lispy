import Text.Parsec ((<|>), char, digit, eof, many1, oneOf, parse, sepBy, spaces, try, Parsec)
import Control.Applicative ((<*))
import System.IO
-- import Debug.Trace (trace)

data Expr = Lit Int |
            Symbol String |
            SVal [Expr] |
            QVal [Expr] |
            Error String deriving Show

data Env = Env [(String, EnvEntry)]

data EnvEntry = EnvEntry (Env -> [Expr] -> Expr)

basicNumber :: Parsec String () Expr
basicNumber = do
  digits <- many1 digit
  return (Lit (read digits :: Int))

negativeNumber :: Parsec String () Expr
negativeNumber = do
  _ <- char '-'
  digits <- many1 digit
  return (Lit ((-1) * (read digits :: Int)))

number :: Parsec String () Expr
number = basicNumber <|> negativeNumber

symbol :: Parsec String () Expr
symbol = do
  result <- many1 (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_+-*/\\=<>!&"))
  return (Symbol result)

sexpr :: Parsec String () Expr
sexpr = do
  _ <- char '('
  result <- sepBy expr spaces
  _ <- char ')'
  return (SVal result)

qexpr :: Parsec String () Expr
qexpr = do
  _ <- char '{'
  result <- sepBy expr spaces
  _ <- char '}'
  return (QVal result)

expr :: Parsec String () Expr
expr = try number <|> symbol <|> sexpr <|> qexpr

program :: Parsec String () Expr
program = do
  result <- (sepBy expr spaces) <* eof
  return (SVal result)

evalSum :: Int -> Int -> Expr
evalSum a b = Lit (a + b)

evalSub :: Int -> Int -> Expr
evalSub a b = Lit (a - b)

evalMul :: Int -> Int -> Expr
evalMul a b = Lit (a * b)

evalDiv :: Int -> Int -> Expr
evalDiv _ 0 = Error "division by zero"
evalDiv a b = Lit (a `div` b)

evalBinOp :: Env -> (Int -> Int -> Expr) -> Expr -> Expr -> Expr
evalBinOp _ _ (Error e) _ = Error e
evalBinOp _ _ _ (Error e) = Error e
evalBinOp _ f (Lit a) (Lit b) = f a b
evalBinOp e f (Lit a) x = evalBinOp e f (Lit a) (eval e x)
evalBinOp _ _ _ _ = Error "invalid arguments to binary operator"

sumOp :: Env -> [Expr] -> Expr
sumOp env exprs = foldl (evalBinOp env evalSum) (Lit 0) exprs

subOp :: Env -> [Expr] -> Expr
subOp env exprs = foldl (evalBinOp env evalSub) (head exprs) (tail exprs)

mulOp :: Env -> [Expr] -> Expr
mulOp env exprs = foldl (evalBinOp env evalMul) (Lit 1) exprs

divOp :: Env -> [Expr] -> Expr
divOp env exprs = foldl (evalBinOp env evalDiv) (head exprs) (tail exprs)

headBuiltin :: Env -> [Expr] -> Expr
headBuiltin _ [QVal []] = Error "head requires a list of at least one element"
headBuiltin _ [QVal (x:_)] = QVal [x]
headBuiltin _ _ = Error "head can only operate on q-expressions"

tailBuiltin :: Env -> [Expr] -> Expr
tailBuiltin _ [QVal []] = Error "tail requires a list of at least one element"
tailBuiltin _ [QVal (_:xs)] = QVal xs
tailBuiltin _ _ = Error "tail can only operate on q-expressions"

listBuiltin :: Env -> [Expr] -> Expr
listBuiltin _ xs = QVal xs

evalBuiltin :: Env -> [Expr] -> Expr
evalBuiltin _ [QVal []] = Error "eval requires a list of at least one element"
evalBuiltin e [QVal xs] = eval e (SVal xs)
evalBuiltin _ _ = Error "eval can only operate on q-expressions"

joinQVals :: Expr -> Expr -> Expr
joinQVals (QVal a) (QVal b) = QVal (a ++ b)
joinQVals _ _ = Error "Can not join non q-expressions"

joinBuiltin :: Env -> [Expr] -> Expr
joinBuiltin _ [] = Error "join requires at least two elements"
joinBuiltin _ [_] = Error "join requires at least two elements"
joinBuiltin _ x = foldl joinQVals (QVal []) x

getEnv :: Env -> String -> Maybe EnvEntry
getEnv (Env []) _ = Nothing
getEnv (Env ((a, f):rest)) b = if a == b then Just f else getEnv (Env rest) b

applyEnv :: Env -> String -> [Expr] -> Expr
-- applyEnv _ k ex | trace ("applyEnv " ++ show k ++ " " ++ show ex) False = undefined
applyEnv e k ex = case (getEnv e k ) of
  Nothing -> Error ("Symbol '" ++ k ++ "' not found in environment")
  Just (EnvEntry f) -> f e ex

evalSVal :: Env -> [Expr] -> Expr
-- evalSVal _ x | trace ("evalSVal " ++ show x) False = undefined
evalSVal _ [] = SVal []
evalSVal e [x] = eval e x
evalSVal e ((Symbol s):xs) = applyEnv e s [eval e x | x <- xs]
evalSVal _ _ = Error "First argument of an sval must be a symbol"

eval :: Env -> Expr -> Expr
eval env (SVal x) = evalSVal env x
eval _ x = x

printExpr :: Expr -> String
printExpr (Lit a) = show a
printExpr (Symbol s) = show s
printExpr (SVal x) = "(" ++ unwords (map printExpr x) ++ ")"
printExpr (QVal x) = "{" ++ unwords (map printExpr x) ++ "}"
printExpr (Error e) = "Error: " ++ e

main :: IO ()
main = do
  putStr "lispy> "
  hFlush stdout
  input <- getLine
  putStrLn (case parse program "(source)" input of
                 Left _ -> "Error parsing input"
                 Right ast -> (printExpr (eval env ast))
                   where env = Env [("+", EnvEntry sumOp),
                                    ("-", EnvEntry subOp),
                                    ("*", EnvEntry mulOp),
                                    ("/", EnvEntry divOp),
                                    ("head", EnvEntry headBuiltin),
                                    ("tail", EnvEntry tailBuiltin),
                                    ("list", EnvEntry listBuiltin),
                                    ("eval", EnvEntry evalBuiltin),
                                    ("join", EnvEntry joinBuiltin)])
  main
