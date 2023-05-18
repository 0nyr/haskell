module Main where

import DC          -- divide & conquer skeleton
import Data.List
import Debug.Trace

-- example of using the DC skeleton

-- quicksort :: Ord a => [a] -> [a]
-- quicksort = dc p b d c
--     where 
--         p xs = length xs < 2
--         b xs = xs
--         d (x:xs) = let (a,b) = partition (<x) xs
--             in [a,b]
--         c (x:_) [a,b] = a ++ (x : b)

-- 1. p::(a->Bool) answers whether the problem can be solved directly.
-- 2. b::(a->b) solves the problem directly.
-- 3. d::(a->[a]) divides the problem into a list of subproblems.
-- 4. c::(a->[b]->b) combines the solutions of the subproblems.

splitInMiddle :: [a] -> ([a], [a])
splitInMiddle list = splitAt (length list `div` 2) list

-- From Wikipedia: An example of merge sort. First, divide the list into 
-- the smallest unit (1 element), then compare each element with the 
-- adjacent list to sort and merge the two adjacent lists. 
-- Finally, all the elements are sorted and merged.
mergesort :: (Ord a, Show a) => [a] -> [a]
mergesort list = dc p b d c list
    where
        -- handle trivial case 
        p list = length list < 2
        b list = list
        -- divide step:  partitioning of the list to be sorted 
        -- in two lists whose lengths differ by at most 1.
        d list = let (leftSublist, rightSublist) = splitInMiddle list
            in
                [leftSublist, rightSublist]
            where
                splitInMiddle :: [a] -> ([a], [a])
                splitInMiddle list = splitAt (length list `div` 2) list
        -- conquer step: combines two (now sorted) sublists to a
        -- sorted list
        c _ [leftSublist, []] = leftSublist
        c _ [[], rightSublist] = rightSublist
        c _ [[a], [b]] = if a <= b then [a, b] else [b, a]
        c _ subproblem =
            trace (show leftSublist ++ ", " ++ show rightSublist ++ ", ") $
            compAndAdd [] leftSublist rightSublist
            where
                [leftSublist, rightSublist] = subproblem
                compAndAdd :: (Ord a, Show a) => [a] -> [a] -> [a] -> [a]
                compAndAdd accum [] [] = accum
                compAndAdd accum [a] [] = accum ++ [a]
                compAndAdd accum [] [b] = accum ++ [b]
                compAndAdd accum listL listR = 
                    trace (show a ++ ", " ++ show b ++ ", ") $
                    if a <= b then compAndAdd (accum ++ [a]) la listR
                    else compAndAdd (accum ++ [b]) listL lb
                    where
                        (a:la) = listL
                        (b:lb) = listR



myAssert :: Bool -> IO ()
myAssert True = putStrLn "🟢 assertion ok"
myAssert False = putStrLn "🔴 assertion failed"

main :: IO ()
main = do
    putStrLn "Week 5 exercices: [see below]"

    let testList = [3, 9, 4, 7, -2, 8, 5, 1]
    print testList

    print(splitInMiddle testList)
    print(splitInMiddle testList)

    putStr "sort testList: "
    print (sort testList)
    putStr "mergesort testList: "
    print (mergesort testList)

    myAssert (mergesort testList == sort testList)