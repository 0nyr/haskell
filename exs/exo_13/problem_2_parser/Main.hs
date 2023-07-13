module Main where

import Expr
import Parser

-- Try
--
-- (Y (\fib -> \n -> if n then if n-1 then fib (n-1) + fib (n-2) else 1 else 0)) 12
--
-- as input. This should give fib(12), i.e., 144 as output.

fib12 :: String
fib12 = "(Y (\\fib -> \\n -> if n then if n-1 then fib (n-1) + fib (n-2) else 1 else 0)) 12"

main :: IO ()
main = do
  input <- getLine
  print (run (parseExpr input))
