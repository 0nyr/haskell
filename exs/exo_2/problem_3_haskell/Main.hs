module Main where

main :: IO ()
main = do
    putStrLn "Problem 3: [see below]" 

    putStr "(a) [0::Integer,2^60 ..]: "
    -- print [0::Integer,2^60 ..] 
    putStrLn "Infinite number (no maxBound) of Integer (java BigInt), with a step of 2^60"

    -- print max value of type Int
    putStr "maxBound :: Int: "
    print (maxBound :: Int)

    putStr "(b) [0::Int,2^28 ..]: "
    -- print [0::Int,2^28 ..]
    putStrLn "Very large (not infinite) number of fixed size 64bits integer, with a step of 2^28"

    putStr "(c) [0::Int,2^31 ..]: "
    -- print [0::Int,2^31 ..] 
    putStrLn "Very large (not infinite) number of fixed size 64bits integer, with a step of 2^31"

    putStr "(d) [0::Int,2^32 ..]: "
    -- print [0::Int,2^32 ..]
    putStrLn "Very large (not infinite) number of fixed size 64bits integer, with a step of 2^32"

    putStr "(e) [0::Int,2^60 ..]: "
    -- print [0::Int,2^60 ..]
    putStrLn "Short list, ends at maxbound of fixed size 64bits integer: [0,1152921504606846976,2305843009213693952,3458764513820540928,4611686018427387904,5764607523034234880,6917529027641081856,8070450532247928832]"

    putStr "(f) [0::Int,2^63 ..]: "
    -- print [0::Int,2^63 ..]
    putStrLn "2 values list, first 0, then ends at maxbound of fixed size 64bits integer: [0, -9223372036854775808]"

    putStr "(g) [0::Int,2^64 ..]: "
    -- print [0::Int,2^64 ..]
    putStrLn "Infinite list of fixed size 64bits integer (infite number of 0), with a step of 2^64 (never reaches maxBound)"

    -- maxBound of type Double
    putStr "maxBound :: Double ??? "
    putStrLn "The numbers represented by Double are bounded, but the values in the type Double are not. The maxBound of type Double is not defined."

    putStr "(h) [0::Double,10^305 ..]: "
    -- print [0::Double,10^305 ..]
    putStrLn "Infinite list of Double (infite number of Infinity). Not a bounded type, so no maxBound, so infinite list."

    putStr "(i) [0.0,0.1 .. 1.0]: "
    print [0.0,0.1 .. 1.0]
    putStrLn "Short list from 0.0 to 1.0, with a step of 0.1. With rounding errors."
