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

addExpr :: Parsec.Parsec String () Expr
addExpr = do
  _ <- Parsec.char '+'
  _ <- Parsec.spaces
  exprList <- Parsec.sepBy expr Parsec.spaces
  return (Add exprList)

subExpr :: Parsec.Parsec String () Expr
subExpr = do
  _ <- Parsec.char '-'
  _ <- Parsec.spaces
  exprList <- Parsec.sepBy expr Parsec.spaces
  return (Sub exprList)

mulExpr :: Parsec.Parsec String () Expr
mulExpr = do
  _ <- Parsec.char '*'
  _ <- Parsec.spaces
  exprList <- Parsec.sepBy expr Parsec.spaces
  return (Mul exprList)

divExpr :: Parsec.Parsec String () Expr
divExpr = do
  _ <- Parsec.char '/'
  _ <- Parsec.spaces
  exprList <- Parsec.sepBy expr Parsec.spaces
  return (Div exprList)

expr :: Parsec.Parsec String () Expr
expr =  numberExpr
   Parsec.<|> addExpr
   Parsec.<|> subExpr
   Parsec.<|> mulExpr
   Parsec.<|> divExpr

parse rule text = Parsec.parse rule "(source)" text

parseInput :: [Char] -> [Char]
parseInput x = x

main :: IO ()
main = do
  putStr "lispy> "
  hFlush stdout
  input <- getLine
  putStrLn (parseInput input)
  main
