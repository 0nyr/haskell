module Main where

import Syntax
import Reduce

{- The fixed point functional of the factorial function as LExp -}
fpfac :: LExp
fpfac = (L "fac" (L "n" 
        (If (Prim OEq [V "n",CInt 0]) 
            (CInt 1) 
            (Prim OMul [V "n",(V "fac") :@: (Prim OSub [V "n",CInt 1])]))))

{- 5 factorial as LExp -}
fac :: LExp
fac = (Y fpfac) :@: CInt 5

test :: String
test = "Reduction to WHNF: " ++ show (last (reduce_WHNF fac)) ++ "\n"



-- Tests
myAssert :: Bool -> IO ()
myAssert True = putStrLn "🟢 assertion ok"
myAssert False = putStrLn "🔴 assertion failed"

-- Check if all elements in a list are equal
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

main :: IO ()
main = do
    

    -- a) fv and bv tests
    let
        -- x    | fv = {x}, bv = {}
        exp1 = V "x"

        -- λx.x    | fv = {}, bv = {x}
        exp2 = L "x" (V "x")

        -- λx.y    | fv = {y}, bv = {x}
        exp3 = L "x" (V "y")

        -- λx.(x y)    | fv = {y}, bv = {x}
        exp4 = L "x" (V "x" :@: V "y")

        -- λx.λy.(x y) = λxy.(x y)    | fv = {}, bv = {x,y}
        exp5 = L "x" (L "y" (V "x" :@: V "y")) 

        -- λx.λy.(x z) = λxy.(x z)    | fv = {z}, bv = {x,y}
        exp6 = L "x" (L "y" (V "x" :@: V "z"))


    -- fv tests
    myAssert $ allEqual [fv exp1, ["x"]]
    myAssert $ allEqual [fv exp2, []]
    myAssert $ allEqual [fv exp3, ["y"]]
    myAssert $ allEqual [fv exp4, ["y"]]
    myAssert $ allEqual [fv exp5, []]
    myAssert $ allEqual [fv exp6, ["z"]]

    -- bv tests
    myAssert $ allEqual [bv exp1, []]
    myAssert $ allEqual [bv exp2, ["x"]]
    myAssert $ allEqual [bv exp3, ["x"]]
    myAssert $ allEqual [bv exp4, ["x"]]
    myAssert $ allEqual [bv exp5, ["y", "x"]]
    myAssert $ allEqual [bv exp6, ["y", "x"]]
    print (bv exp5)
    print (bv exp6)

    -- occursNot tests
    myAssert $ occursNot ["x", "y", "z"] == "x0"
    -- print (occursNot ["x", "y", "z"])
    myAssert $ occursNot ["x", "y", "z", "x0", "x1"] == "x2"
    -- print (occursNot ["x", "y", "z", "x0", "x1"])

    -- substitute tests
    let
        -- λx.x [x:=y] = λz.z (since we can't directly replace x with y in λx.x)
        exp7 = substitute "x" (V "y") exp2
        correctExp7 = L "z" (V "z")
    myAssert $ exp7 == correctExp7
    print exp7
    print correctExp7

    -- (x y) [x:=λz.z] = (λz.z y)
    let
        exp8 = substitute "x" (L "z" (V "z")) (V "x" :@: V "y")
        correctExp8 = (L "z" (V "z")) :@: V "y"
    myAssert $ exp8 == correctExp8

    -- d)   
    putStr test
    -- myAssert $ test == CInt 120