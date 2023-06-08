module Main where


hom :: (a->b) -> (b->b->b) -> b -> [a] -> b
hom f op e = foldr op e . map f

-- a)
sqrSumH :: Num a => [a] -> a
sqrSumH = hom (^2) (+) 0

-- b)
concatH :: [[a]] -> [a]
concatH = hom id (++) []

-- c)
composeH :: [a->a] -> a -> a
composeH = hom id (.) id

-- d)
insertInAscendingOrder :: Ord a => [a] -> [a] -> [a]
insertInAscendingOrder [x] [] = [x]
insertInAscendingOrder [x] (y:ys)
    | x <= y = x:y:ys
    | otherwise = y : insertInAscendingOrder [x] ys
insertInAscendingOrder _ _ = error "insertInAscendingOrder: first argument must be a singleton list"

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