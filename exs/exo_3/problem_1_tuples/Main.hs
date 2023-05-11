module Main where

import Test.HUnit

ascendingSort2 :: Ord a => a -> a -> (a, a)
ascendingSort2 a b
    | aIsGreater = (b, a)
    | not aIsGreater = (a, b)
    where aIsGreater = a > b

ascendingSort2Correction :: Ord a => a -> a -> (a, a)
ascendingSort2Correction x y
    | x < y = (x, y)
    | otherwise = (y, x)

-- return a tuple ordered in ascending order
ascendingSort3 :: Ord a => a -> a -> a -> (a, a, a)
ascendingSort3 a b c =
    let resLeft = ascendingSort2 a b == (a, b)
        resRigth = ascendingSort2 b c == (b, c)
        resBorder = ascendingSort2 a c == (a, c)
    -- WARN: ordering of a, b, c is a combination, so 6 possible cases
    -- However, 2 binaries values (True or False) in 6 cells is a 
    -- permutation, and has 8 possible cases.
    -- This means 2 permutations here are IMPOSSIBLE
    -- Case (True, True, False) and (False, False, True) are impossible
    in case (resLeft, resRigth, resBorder) of
        (True, True, True) -> (a, b, c)
        (True, False, True) -> (a, c, b)
        (False, True, True) -> (b, a, c)
        (False, True, False) -> (b, c, a)
        (True, False, False) -> (c, a, b)
        (False, False, False) -> (c, b, a)
    
ascendingSort3Correction :: Ord a => a -> a -> a -> (a, a, a)
ascendingSort3Correction x y z =
    let (k, l) = ascendingSort2 x y -- use patern matching, or function snd (second)
    in let (m, u) = ascendingSort2 l z -- determine max, u
        in let (s, t) = ascendingSort2 k m -- determine min and intermediate value
            in (s, t, u)

-- improved correction
ascendingSort3Correction2 :: Ord a => a -> a -> a -> (a, a, a)
ascendingSort3Correction2 x y z = (minVal, midVal, maxVal)
  where 
    (lowerXY, upperXY) = ascendingSort2 x y
    (lowerFinal, maxVal) = ascendingSort2 upperXY z
    (minVal, midVal) = ascendingSort2 lowerXY lowerFinal



-- test ascendingSort3
test1 = TestCase (assertEqual "Test case 1" (1, 2, 3) (ascendingSort3 1 2 3))
test2 = TestCase (assertEqual "Test case 2" (1, 2, 3) (ascendingSort3 1 3 2))
test3 = TestCase (assertEqual "Test case 3" (1, 2, 3) (ascendingSort3 2 1 3))
test4 = TestCase (assertEqual "Test case 4" (1, 2, 3) (ascendingSort3 2 3 1))
test5 = TestCase (assertEqual "Test case 5" (1, 2, 3) (ascendingSort3 3 1 2))
test6 = TestCase (assertEqual "Test case 6" (1, 2, 3) (ascendingSort3 3 2 1))

tests = TestList [test1, test2, test3, test4, test5, test6]


main :: IO ()
main = do
    putStrLn "Week 3 exercices: [see below]"

    -- ascendingSort2
    putStr "ascendingSort2 1 2: "
    print (ascendingSort2 1 2)

    putStr "ascendingSort2 3 2: "
    print (ascendingSort2 2 3)

    putStr "ascendingSort2 -3 2: "
    print (ascendingSort2 (-3) 2)

    -- ascendingSort3
    putStr "ascendingSort3 -1 2 5: "
    print (ascendingSort3 (-1) 2 5)

    -- run the test cases and print the results
    putStrLn "\n-- Test results --"
    testResults <- runTestTT tests
    print testResults

    

