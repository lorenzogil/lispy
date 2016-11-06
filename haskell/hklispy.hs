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

sumExpr :: Expr -> Expr -> Expr
sumExpr (Error e) _ = Error e
sumExpr (Lit a) (Lit b) = Lit (a + b)
sumExpr (Lit a) (LVal x) = sumExpr (Lit a) (eval (LVal x))
sumExpr _ _ = Error "invalid arguments to Add"

subExpr :: Expr -> Expr -> Expr
subExpr (Error e) _ = Error e
subExpr (Lit a) (Lit b) = Lit (a - b)
subExpr (Lit a) (LVal x) = subExpr (Lit a) (eval (LVal x))
subExpr _ _ = Error "invalid arguments to Sub"

mulExpr :: Expr -> Expr -> Expr
mulExpr (Error e) _ = Error e
mulExpr (Lit a) (Lit b) = Lit (a * b)
mulExpr (Lit a) (LVal x) = mulExpr (Lit a) (eval (LVal x))
mulExpr _ _ = Error "invalid arguments to Mul"

divExpr :: Expr -> Expr -> Expr
divExpr (Error e) _ = Error e
divExpr (Lit a) (Lit b) = Lit (a `div` b)
divExpr (Lit a) (LVal x) = divExpr (Lit a) (eval (LVal x))
divExpr _ _ = Error "invalid arguments to Div"

eval :: Expr -> Expr
eval (Lit a) = Lit a
eval (Error e) = Error e
eval (Symbol s) = Error ("no arguments for symbol " ++ show s)
eval (LVal []) = Error "empty lval"
eval (LVal [x]) = eval x
eval (LVal (Symbol s:exprs)) = case s of
  Add -> foldl sumExpr (Lit 0) exprs
  Sub -> foldl subExpr (head exprs) (tail exprs)
  Mul -> foldl mulExpr (Lit 1) exprs
  Div -> foldl divExpr (head exprs) (tail exprs)
eval (LVal (Error e:_)) = Error e
eval _ = Error "invalid expression"

main :: IO ()
main = do
  putStr "lispy> "
  hFlush stdout
  input <- getLine
  putStrLn (case parse program "(source)" input of
                 Left _ -> "Error parsing input"
                 Right ast -> show (eval ast))
  main

