import System.IO

main :: IO ()
main = do
  putStr "lispy> "
  hFlush stdout
  input <- getLine
  putStrLn ("No you're a " ++ input)
  main
