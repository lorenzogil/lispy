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

expr :: Parsec.Parsec String () Expr
expr =  numberExpr Parsec.<|> opExpr

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
