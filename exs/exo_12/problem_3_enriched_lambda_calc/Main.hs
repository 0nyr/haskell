module Main where

import Syntax
import Eval
import Environment
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

{- Comparison of reduction and denotational semantics -}
test :: String
test =  "reduction semantics: " ++ show (last (reduce_WHNF fac)) ++ "\n"
     ++ "denotational semantics: " ++ show (eval fac emptyEnv) ++ "\n" 

main :: IO ()
main = putStr test
