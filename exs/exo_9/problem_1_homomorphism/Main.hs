module Main where

import Debug.Trace
import Data.Type.Coercion (trans)

-- we define the following h function:
hom :: (a->b) -> (b->b->b) -> b -> [a] -> b
hom f op e = foldr op e . map f
-- h is a list hommorphism

-- correction notes
-- h :: [a] -> b is a list homomorphism
-- by definition:
--      there exists an associative operator # with
--      a neutral element e such that:
--      i) h (xs ++ ys) = h xs # h ys
--      ii) h [] = e
-- First homomorphism law:
-- there exists a function f and an associative function op
-- with neutral element e such that:
--      h = foldr op e . map f
-- Second and Third homomorphism law:
-- there exists functions b, c and values k, l such that:
--      h = foldr op e . map f
--      h = foldl op e
--      h  h (xs ++ ys) = h xs # h ys


-- a)
revertListH :: [a] -> [a]
revertListH [] = []
revertListH [x] = [x]
revertListH list = 
    hom (:[]) opH [] list
    where
        opH :: [a] -> [a] -> [a]
        opH xs ys = ys ++ xs

-- twistH "ABCDEF" ⇝ "BADCFE"
-- twistH "ABCDEFG" ⇝ "BADCFEG"
-- twistH :: [a] -> [a]
-- twistH [] = []
-- twistH [x] = [x]
-- twistH list = 
--     hom (:[]) opH [] list
--     where
--         opH :: [a] -> [a] -> [a]
--         opH [xs] acc 
--             | (length acc) `div` 2 == 1 = head acc : xs : tail acc
--             | otherwise = xs : acc


-- WARN: the `div` operator is the integer division, it rounds the result
twistHShow :: Show a => [a] -> [a]
twistHShow [] = []
twistHShow [x] = [x]
twistHShow list = 
    hom (:[]) opH [] list
    where
        opH :: Show a => [a] -> [a] -> [a]
        opH [xs] acc 
            | traceShow ("Length of acc: " ++ show (length acc) ++ ", len(acc) mod 2 = " ++ show ((length acc) `mod` 2)) (((length acc) `mod` 2) == 0) = 
                traceShow ("First branch: acc=" ++ show acc ++ ", xs=" ++ show xs) (xs : acc)
            | otherwise = 
                traceShow ("Second branch: acc=" ++ show acc ++ ", xs=" ++ show xs) (head acc : xs : tail acc)

twistH :: [a] -> [a]
twistH [] = []
twistH [x] = [x]
twistH list = 
    hom (:[]) opH [] list
    where
        opH :: [a] -> [a] -> [a]
        opH [xs] acc 
            | (length acc) `mod` 2 == 0 = xs : acc
            | otherwise = head acc : xs : tail acc

-- correction: we could have defined the operator as:
us # vs = if even (length us) then us ++ vs
            else init us ++ twist (last us : twist vs)

-- b)
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose xss = [x | (x:_) <- xss] : transpose [xs | (_:xs) <- xss]

opTransposeH :: [[a]] -> [[a]] -> [[a]]
opTransposeH [] [] = []
opTransposeH (xHeadSublist:xss) [] = xHeadSublist : opTransposeH xss []
opTransposeH [] (yHeadSublist:yss) = yHeadSublist : opTransposeH [] yss
opTransposeH (xHeadSublist:xss) (yHeadSublist:yss) = (xHeadSublist ++ yHeadSublist) : opTransposeH xss yss

transposeH :: [[a]] -> [[a]]
transposeH [] = []
transposeH listOflist = hom f opTransposeH [[]] listOflist
    where
        -- f should convert each element of type [a] (a list) 
        -- into a type [[a]] (a list of lists).
        -- Each element of the input list is itself a list, and to 
        -- meet the output type, we should convert each element 
        -- into a list of singleton lists
        f :: [a] -> [[a]]
        f = map (:[])

-- Tests
myAssert :: Bool -> IO ()
myAssert True = putStrLn "🟢 assertion ok"
myAssert False = putStrLn "🔴 assertion failed"

main :: IO ()
main = do
    putStrLn "Week 9 exercices: [see below]"

    -- a)
    putStr "revertListH 'ABCDEF' (expected: 'FEDCBA'):"
    print (revertListH "ABCDEF")
    myAssert (revertListH "ABCDEF" == "FEDCBA")

    putStr "twistH 'ABCDEF' (expected: 'BADCFE'):"
    print (twistH "ABCDEF")
    myAssert (twistH "ABCDEF" == "BADCFE")

    -- b)
    putStrLn ("[[1,4], [2,5], [3]] `opTransposeH` [[6], [7], [8], [9]] (expected: [[1,4,6],[2,5,7],[3,8],[9]]): ")
    print([[1,4], [2,5], [3]] `opTransposeH` [[6], [7], [8], [9]])
    myAssert ([[1,4], [2,5], [3]] `opTransposeH` [[6], [7], [8], [9]] == [[1,4,6],[2,5,7],[3,8],[9]])

    -- example 1
    putStrLn ("transpose [[1,2,3],[4,5,6]] (expected: [[1,4],[2,5],[3,6]]): ")
    print (transpose [[1,2,3],[4,5,6]])
    myAssert (transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]])

    -- example 2
    putStrLn ("transpose [[1,2,3],[4,5],[],[6,7,8,9]] (expected: [[1,4,6],[2,5,7],[3,8],[9]]): ")
    print (transpose [[1,2,3],[4,5],[],[6,7,8,9]])
    myAssert (transpose [[1,2,3],[4,5],[],[6,7,8,9]] == [[1,4,6],[2,5,7],[3,8],[9]])

