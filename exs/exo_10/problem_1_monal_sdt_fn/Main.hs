module Main where


import Control.Monad

-- a)
concatM :: [[a]] -> [a]
-- concatM [] = return []
--concatM list = list >>= (\y -> return y)
-- The following >>= is equivalent to `xs >>= f = concat (map f xs)`
-- so here we are just reusing the built-in bind operator of lists
concatM listOfList = listOfList >>= id

concatMDo :: [[a]] -> [a]
concatMDo listOfList = do
    y <- listOfList
    y

-- b)
mapMonad :: (a -> b) -> [a] -> [b]
mapMonad f list = list >>= (\y -> return (f y))

mapMonadDo :: (a -> b) -> [a] -> [b]
mapMonadDo f list = do
    y <- list
    return (f y)

-- c)
filterMonad :: (a -> Bool) -> [a] -> [a]
filterMonad f list = list >>= (\y -> if f y then return y else [])

filterMonadDo :: (a -> Bool) -> [a] -> [a]
filterMonadDo f list = do
    y <- list
    if f y then return y else []

-- Tests
myAssert :: Bool -> IO ()
myAssert True = putStrLn "🟢 assertion ok"
myAssert False = putStrLn "🔴 assertion failed"

main :: IO ()
main = do
    putStrLn "Week 10 exercices: [see below]"

    -- a)
    putStr "concatM [[1,2,3],[4,5,6],[7,8,9]] (expected: [1,2,3,4,5,6,7,8,9]):"
    print (concatM [[1,2,3],[4,5,6],[7,8,9]])
    myAssert (concatM [[1,2,3],[4,5,6],[7,8,9]] == [1,2,3,4,5,6,7,8,9])

    putStr "concatMDo [[1,2,3],[4,5,6],[7,8,9]] (expected: [1,2,3,4,5,6,7,8,9]):"
    print (concatMDo [[1,2,3],[4,5,6],[7,8,9]])
    myAssert (concatMDo [[1,2,3],[4,5,6],[7,8,9]] == [1,2,3,4,5,6,7,8,9])

    -- b)
    putStr "mapM (+1) [1,2,3] (expected: [2,3,4]):"
    print (mapMonad (+1) [1,2,3])
    myAssert (mapMonad (+1) [1,2,3] == [2,3,4])

    putStr "mapMonadDo (+1) [1,2,3] (expected: [2,3,4]):"
    print (mapMonadDo (+1) [1,2,3])
    myAssert (mapMonadDo (+1) [1,2,3] == [2,3,4])

    -- c)
    putStr "filterMonad (>2) [1,2,3,4,5] (expected: [3,4,5]):"
    print (filterMonad (>2) [1,2,3,4,5])
    myAssert (filterMonad (>2) [1,2,3,4,5] == [3,4,5])

    putStr "filterMonadDo (>2) [1,2,3,4,5] (expected: [3,4,5]):"
    print (filterMonadDo (>2) [1,2,3,4,5])
    myAssert (filterMonadDo (>2) [1,2,3,4,5] == [3,4,5])