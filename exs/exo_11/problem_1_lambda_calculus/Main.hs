module Main where

lambdaC :: p -> p
lambdaC = ((\f -> f.f) (\ x y -> x)) (\z -> z)
lambdaCComputed = (\ x y -> x)

main :: IO ()
main = do
    putStrLn "(\\f -> f.f)(\\x -> (\\y -> x))(\\z -> z): [see below]"
    print (lambdaC 10)