module Main where

import Data.List

-- All the paper proofs like:
--  + finding function most general type
--  + determining strictness of arguments
--  + proofs by inductions of properties of functions
--  + proof by contradiction that a function is not an homomorphism
-- are in the paper exam.

-- Calculator is not needed for the exam.

-- In first exercices, you need to evaluate by hand some expressions 
-- 1)
computeSillyList = [a + b | a <- [3..5], odd a, b <- [a..7]]

-- 2)
computeSillyFoldl = foldl (-) 4 [4,5,6]

computeSillyFoldr = foldr (-) 4 [4,5,6]

-- 3)
computeSillyMap :: [(Integer, Maybe Integer)]
computeSillyMap = map g [1..5]
    where
        g x = (x*x + 1, Just x)

-- The following exercices are to find some expression in order 
-- to rewrite some functions in terms of other functions.
-- 4)
function4 :: (b -> a -> b) -> b -> [a] -> [b]
function4 f z xs = scanl f z xs

function4' :: (b -> a -> b) -> b -> [a] -> [b]
function4' f z xs = map delta (inits xs) 
    where
        delta ys = foldl f z ys

-- Following questions are about writing functions
-- 5)
checkIsOrdered :: Ord a => [a] -> Bool
checkIsOrdered [] = True
checkIsOrdered [x] = True
-- checkIsOrdered (x:y:xs) = (x <= y) && checkIsOrdered (y:xs)
checkIsOrdered (x:y:xs) = if x <= y then checkIsOrdered (y:xs) else False

-- 6) Insert at position
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x xs = x:xs
insertAt n x (y:ys) = y : insertAt (n-1) x ys

-- 7) Write a function that return the list of permutations of a list
-- example: myPermutations [1,2] = [[1,2],[2,1]]
-- example: myPermutations [1,2,3] = [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
myPermutations :: [a] -> [[a]]
myPermutations [] = [[]]
myPermutations (x:xs) = concatMap (insertAtAll x) (myPermutations xs)
    where
        insertAtAll x [] = [[x]]
        insertAtAll x (y:ys) = (x:y:ys) : map (y:) (insertAtAll x ys)

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
    putStrLn "Week 8 exercices: [see below]"

    -- 1)
    putStr "Q1:"
    print computeSillyList
    myAssert (computeSillyList == [3 + 3, 3 + 4, 3 + 5, 3 + 6, 3 + 7, 5 + 5, 5 + 6, 5 + 7])

    -- 2)
    putStr "Q2, a:"
    print computeSillyFoldl
    myAssert (computeSillyFoldl == ((4 - 4) - 5) - 6)

    putStr "Q2, b:"
    print computeSillyFoldr
    myAssert (computeSillyFoldr == 4 - (5 - (6 - 4)))

    -- 3)
    putStr "Q3:"
    print computeSillyMap
    myAssert (computeSillyMap == [(2,Just 1),(5,Just 2),(10,Just 3),(17,Just 4),(26,Just 5)])

    -- 4)
    putStr "Q4:"
    print (function4 (+) 0 [1..5])
    print (function4' (+) 0 [1..5])
    myAssert (function4 (+) 0 [1..5] == function4' (+) 0 [1..5])

    -- 5)
    putStr "Q5:"
    print (checkIsOrdered [1,2,3,4,5])
    myAssert (checkIsOrdered [1,2,3,4,5] == True)
    myAssert (checkIsOrdered [1,2,3,4,5,4] == False)

    -- 6)
    putStr "Q6:"
    print (insertAt 2 0 [1,2,3,4,5])
    myAssert (insertAt 2 0 [1,2,3,4,5] == [1,2,0,3,4,5])
    myAssert (insertAt 0 0 [1,2,3,4,5] == [0,1,2,3,4,5])
    myAssert (insertAt 5 0 [1,2,3,4,5] == [1,2,3,4,5,0])

    -- 7)
    putStr "Q7:"
    print (myPermutations [1,2])
    print (myPermutations [1,2,3])
    myAssert (myPermutations [1,2] == [[1,2],[2,1]])
    myAssert (myPermutations [1,2,3] == [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]])



