import qualified Text.Parsec as Parsec
import System.IO

data Expr = Lit Int
          | Add [Expr]
          | Sub [Expr]
          | Mul [Expr]
          | Div [Expr]
          deriving Show

basicNumber :: Parsec.Parsec String () Expr
basicNumber = do
  digits <- Parsec.many1 Parsec.digit
  return (Lit (read digits :: Int))

negativeNumber :: Parsec.Parsec String () Expr
negativeNumber = do
  _ <- Parsec.char '-'
  digits <- Parsec.many1 Parsec.digit
  return (Lit ((-1) * (read digits :: Int)))

numberExpr :: Parsec.Parsec String () Expr
numberExpr = basicNumber Parsec.<|> negativeNumber

opExpr :: Parsec.Parsec String () Expr
opExpr = do
  op <- Parsec.oneOf "+-*/"
  _ <- Parsec.spaces
  exprList <- Parsec.sepBy1 expr Parsec.spaces
  return (case op of '+' -> Add exprList
                     '-' -> Sub exprList
                     '*' -> Mul exprList
                     '/' -> Div exprList)

parensExpr :: Parsec.Parsec String () Expr
parensExpr = do
  _ <- Parsec.char '('
  result <- opExpr
  _ <- Parsec.char ')'
  return result

expr :: Parsec.Parsec String () Expr
expr =  numberExpr Parsec.<|> parensExpr

program :: Parsec.Parsec String () Expr
program = opExpr

parse :: String -> Maybe Expr
parse text = case Parsec.parse program "(source)" text of
                  Left _ -> Nothing
                  Right e -> Just e

eval :: Expr -> Double
eval ast = case ast of
  Lit n -> fromIntegral n
  Add exprList -> foldl (\acc i -> acc + eval i) 0 exprList
  Sub exprList -> foldl (\acc i -> acc - eval i) (eval (head exprList)) (tail exprList)
  Mul exprList -> foldl (\acc i -> acc * eval i) 1 exprList
  Div exprList -> foldl (\acc i -> acc / eval i) (eval (head exprList)) (tail exprList)

main :: IO ()
main = do
  putStr "lispy> "
  hFlush stdout
  input <- getLine
  putStrLn (case parse input of
                 Nothing -> "Error parsing input"
                 Just ast -> show (eval ast))
  main

