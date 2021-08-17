module Main where

main :: IO ()
main = do
  x <- getLine
  let y = read x :: [(Int, [Double])]
  print y
