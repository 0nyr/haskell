module Main where

import Control.DeepSeq

-- The strict version of foldl for the sum operation
foldlStrictForSum :: (Num a) => a -> [a] -> a
foldlStrictForSum acc [] = acc
foldlStrictForSum acc (x:xs) = let acc' = acc + x in acc' `seq` foldlStrictForSum acc' xs

-- The strict version of foldl
foldlStrict :: (NFData a, NFData b) => (a -> b -> a) -> a -> [b] -> a
foldlStrict f acc [] = acc
foldlStrict f acc (x:xs) = let acc' = f acc x in acc' `deepseq` foldlStrict f acc' xs

-- Tests
myAssert :: Bool -> IO ()
myAssert True = putStrLn "🟢 assertion ok"
myAssert False = putStrLn "🔴 assertion failed"

main :: IO ()
main = do
    -- tests for foldlStrictForSum...
    let result1 = foldlStrictForSum 0 [1..10^7] :: Integer
    let result2 = sum [1..10^7] :: Integer
    myAssert $ result1 == result2

    -- tests for foldlStrict...
    let result3 = foldlStrict (+) 0 [1..10^7] :: Integer
    let result4 = sum [1..10^7] :: Integer
    myAssert $ result3 == result4
