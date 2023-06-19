module Main where

-- # So, what is a homomorphism?
-- A homomorphism is a structure-preserving map 
-- between two algebraic structures of the same type.
-- It is a map between 2 monoids.
-- A monoid is a set with a binary operation 
-- and an identity element.

-- # example 1:
-- 2 + 3 = 5
-- e^2 * e^3 = e^(2+3) = e^5
-- here, the product to the sum relationship 
-- on exponents is a homomorphism

-- # example 2:
-- length :: [a] -> Int
-- length :: ([a], ++, 0) -> (Int, +, 0)
-- length is a list homomorphism
-- ([a], ++, 0), and (Int, +, 0) are monoids
-- length [] = 0
-- length (xs ++ ys) = length xs + length ys

-- # example 3:
-- h : [a] -> S
-- h : ([a], ++, []) -> (S, *, e)
-- h is a list homomorphism
-- ([a], ++, []) and (S, *, e) are monoids
-- h [] = e
-- h (xs ++ ys) = h xs * h ys

-- # example 4:
-- product :: Num a => [a] -> a
-- product :: ([a], *, 1) -> (a, *, 1)
-- product is a list homomorphism
-- ([a], *, 1) and (a, *, 1) are monoids
-- product [] = 1
-- product (xs ++ ys) = product xs * product ys

-- # example 5:
-- reverse :: [a] -> [a]
-- reverse :: ([a], ++, []) -> ([a], ++, [])
-- reverse is a list homomorphism
-- ([a], ++, []) and ([a], ++, []) are monoids
-- reverse [] = []
-- reverse (xs ++ ys) = reverse ys ++ reverse xs

-- # example 6: merge sort
-- sort :: Ord a => [a] -> [a]
-- sort :: ([a], ++, []) -> ([a], merge, [])
-- sort is a list homomorphism
-- ([a], ++, []) and ([a], merge, []) are monoids
-- sort [] = []
-- sort (xs ++ ys) = merge (sort xs) (sort ys)

-- # example 7: NOT a homomorphism
-- average :: [Int] -> Int
-- avegage (xs ++ ys) = average xs ? average ys
-- average [] = undefined !!!
-- average has no neutral element, 
--      hence it is not a homomorphism

-- let's prove that average is not a homomorphism
-- Assume that average is a homomorphism, i.e.
-- there exists an operator # such that:
-- average (xs ++ ys) = average xs # average ys
-- then:
--    average [1,1,2] = average([1,1] ++ average [2]) 
--    average [1,1,2] = average [1,1] # average [2]
--    average [1,1,2] = 1 # 2
--    average [1,1,2] = average [1] # average [2]
--    average [1,1,2] = average ([1] ++ [2])
--    average [1,1,2] = average [1,2]
-- the upper is false, avg(1,1,2) != avg(1,2) 
-- hence average is not a homomorphism

-- we define the following h function:
hom :: (a->b) -> (b->b->b) -> b -> [a] -> b
hom f op e = foldr op e . map f
-- h is a list hommorphism


-- a)
sqrSumH :: Num a => [a] -> a
sqrSumH = hom (^2) (+) 0

sqrSumHCorrection :: Num a => [a] -> a
sqrSumHCorrection = hom (\x -> x*x) (+) 0

-- b)
concatH :: [[a]] -> [a]
concatH = hom id (++) []

-- c)
composeH :: [a->a] -> a -> a
composeH = hom id (.) id

-- d)
-- WARN: Not enough, insert must be associative
-- this works only for the current implementation of h function
insertInAscendingOrder :: Ord a => [a] -> [a] -> [a]
insertInAscendingOrder [x] [] = [x]
insertInAscendingOrder [x] (y:ys)
    | x <= y = x:y:ys
    | otherwise = y : insertInAscendingOrder [x] ys
insertInAscendingOrder _ _ = error "insertInAscendingOrder: first argument must be a singleton list"

-- corrected version of insertInAscendingOrder
-- merge is associative
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = []
merge xs [] = []
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

sortH :: Ord a => [a] -> [a]
sortH = hom (: []) insertInAscendingOrder []

-- e)
transposeOriginal :: [[a]] -> [[a]]
transposeOriginal [] = []
transposeOriginal ([]:xss) = transposeOriginal xss
transposeOriginal xss = [x | (x:_) <- xss] : transposeOriginal [xs | (_:xs) <- xss]

concat2by2Sublists :: [[a]] -> [[a]] -> [[a]]
concat2by2Sublists [] [] = []
concat2by2Sublists list [] = list
concat2by2Sublists [] list = list
concat2by2Sublists (x:xs) (y:ys) = (x ++ y) : concat2by2Sublists xs ys


-- [[1,2,3], [4,5,6], [7,8,9]]
-- [1,4,7] -> Acc[[],[],[]]
createAccumulator :: [[a]] -> [[a]]
createAccumulator [] = []
createAccumulator (_:xss) = []: createAccumulator xss

operator :: [a] -> [[a]] -> [[a]]
operator [] [] = []
operator (x:xs) [] = [x] : operator xs []
operator [] acc = acc
operator (x:xs) (y:ys) = (x : y) : operator xs ys

operatorForH :: [[a]] -> [[a]] -> [[a]]
operatorForH [] [] = []
operatorForH [list] acc = operator list acc
operatorForH _ _ = error "operatorForH: first argument must be a singleton list of list"

transposeHomomorphism :: [[a]] -> [[a]]
transposeHomomorphism list = hom (:[]) operatorForH (createAccumulator list) list

testMatrix :: [[Int]]
testMatrix = [[1,2,3], [4,5,6], [7,8,9]]

testFalseMatrix :: [[Int]]
testFalseMatrix = [[1,2,3],[4,5],[],[6,7,8,9]]

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

    -- a)
    putStr "sqrSumH [1..3] (expecting 14):"
    print (sqrSumH [1..3])

    -- b)
    putStr "concatH [\"hello\", \" \", \"world\"] (expecting \"hello world\"):"
    print (concatH ["hello", " ", "world"])

    -- c)
    putStr "composeH [(+1), (^2), (*3)] 2 (expecting 37):"
    print (composeH [(+1), (^2), (*3)] 2)

    -- d)
    putStr "sortH [2,5,7,3,1] (expecting [1,2,3,5,7]):"
    print (sortH [2,5,7,3,1])

    -- e)
    putStrLn "transpose functions must be equal:"
    -- assert that the two transpose functions are equal
    myAssert (transposeOriginal testMatrix == transposeHomomorphism testMatrix)
    print (transposeOriginal testMatrix)
    print (transposeHomomorphism testMatrix)

    -- just for experimentation
    -- since we are dealing with non-rectangular matrices, we have no clue how to transpose them
    print (transposeHomomorphism (transposeHomomorphism testFalseMatrix))
    print (transposeOriginal (transposeOriginal testFalseMatrix))
    myAssert (transposeOriginal (transposeOriginal testFalseMatrix) == transposeHomomorphism (transposeHomomorphism testFalseMatrix))