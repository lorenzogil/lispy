import Text.Parsec ((<|>), char, digit, eof, many1, oneOf, parse, sepBy, spaces, try, Parsec)
import Control.Applicative ((<*))
import System.IO

data Symbol = Add | Sub | Mul | Div deriving Show

data Expr = Lit Int | Symbol Symbol | LVal [Expr] | Error String deriving Show

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

symbol:: Parsec String () Expr
symbol = do
  op <- oneOf "+-*/"
  return (case op of '+' -> Symbol Add
                     '-' -> Symbol Sub
                     '*' -> Symbol Mul
                     '/' -> Symbol Div
                     _ -> Error "Bad symbol")

sexpr :: Parsec String () Expr
sexpr = do
  _ <- char '('
  result <- sepBy expr spaces
  _ <- char ')'
  return (LVal result)

expr :: Parsec String () Expr
expr = try number <|> symbol <|> sexpr

program :: Parsec String () Expr
program = do
  result <- (sepBy expr spaces) <* eof
  return (LVal result)

evalSum :: Int -> Int -> Expr
evalSum a b = Lit (a + b)

evalSub :: Int -> Int -> Expr
evalSub a b = Lit (a - b)

evalMul :: Int -> Int -> Expr
evalMul a b = Lit (a * b)

evalDiv :: Int -> Int -> Expr
evalDiv _ 0 = Error "division by zero"
evalDiv a b = Lit (a `div` b)

evalBinOp :: (Int -> Int -> Expr) -> Expr -> Expr -> Expr
evalBinOp _ (Error e) _ = Error e
evalBinOp _ _ (Error e) = Error e
evalBinOp f (Lit a) (Lit b) = f a b
evalBinOp f (Lit a) (LVal x) = evalBinOp f (Lit a) (eval (LVal x))
evalBinOp _ _ _ = Error "invalid arguments to binary operator"

eval :: Expr -> Expr
eval (Lit a) = Lit a
eval (Error e) = Error e
eval (Symbol s) = Error ("no arguments for symbol " ++ show s)
eval (LVal []) = Error "empty lval"
eval (LVal [x]) = eval x
eval (LVal (Symbol s:exprs)) = case s of
  Add -> foldl (evalBinOp evalSum) (Lit 0) exprs
  Sub -> foldl (evalBinOp evalSub) (head exprs) (tail exprs)
  Mul -> foldl (evalBinOp evalMul) (Lit 1) exprs
  Div -> foldl (evalBinOp evalDiv) (head exprs) (tail exprs)
eval (LVal (Error e:_)) = Error e
eval _ = Error "invalid expression"

printExpr :: Expr -> String
printExpr (Lit a) = show a
printExpr (Error e) = "Error: " ++ e
printExpr _ = "Can not print this expression"

main :: IO ()
main = do
  putStr "lispy> "
  hFlush stdout
  input <- getLine
  putStrLn (case parse program "(source)" input of
                 Left _ -> "Error parsing input"
                 Right ast -> printExpr (eval ast))
  main

