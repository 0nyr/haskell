module Main where
import Data.Bool (bool)

data BoolFunction = BF { numberArgs::Int, fn::[Bool]->Bool }


-- create a list of lists which are all combinations of booleans.
-- use recursion, following this example
-- [] -> []
-- [Bool] -> [
--     [True],
--     [False]
-- ]
-- [bool, bool]
-- call 2 times the preceding list, and append either true or false to each sublist
-- [
--     True : [True] ,
--     True : [False],
-- ]
-- [
--     False : [True],
--     False : [False],
-- ]
generateBoolCombinationLists :: Int -> [[Bool]]
generateBoolCombinationLists 0 = [[]]
generateBoolCombinationLists n =
    let precedingRes = generateBoolCombinationLists (n - 1)
        appendBool :: Bool -> [[Bool]] -> [[Bool]]
        appendBool _ [] = []
        -- append bool to current sub list and store the resulting sublist to the rest, after recursion
        -- same as: 
        --      appendBool bool listOfList = map (bool :) listOfList
        appendBool bool (currentSubList:tail) = (bool : currentSubList) : appendBool bool tail
    in appendBool True precedingRes ++ appendBool False precedingRes


-- correction
boolCombs :: Int -> [[Bool]]
boolCombs 0 = [[]]
--boolCombs 1 = [[False], [True]]
boolCombs n = [False : b | b <- bsc] ++ [True : b | b <- bsc]
    where
        bsc = boolCombs (n - 1)

isSatisfiable :: BoolFunction -> Maybe [Bool]
isSatisfiable (BF {numberArgs=0, fn=fn})
    | res = Just []
    | otherwise = Nothing
    where res = fn []
isSatisfiable (BF {numberArgs=n, fn=fn})
    | null listOfRes = Nothing
    | otherwise = Just (head listOfRes)
    where
        allPossibleListsOfBool = generateBoolCombinationLists n
        listOfRes = [ res | res <- allPossibleListsOfBool, fn res ]


-- NB: Older, and less beautiful
-- isSatisfiable boolFunction = 
--     if numberArgs boolFunction == 0 then -- case with empty list as arg for bfun f
--         let res = fn boolFunction [] 
--         in if res then
--             Just []
--         else 
--             Nothing
--     else 
--         let allPossibleListsOfBool = generateBoolCombinationLists (numberArgs boolFunction)
--             listOfRes = [ res | res <- allPossibleListsOfBool, fn boolFunction res ]
--         in case listOfRes of
--             [] -> Nothing
--             _ -> Just (head listOfRes) -- return just first case solution

-- correction
isSatisfiableCorrection :: BoolFunction -> Maybe [Bool]
isSatisfiableCorrection (BF {numberArgs=nA, fn=f}) = 
    case [b | b <- boolCombs nA, f b] of
        [] -> Nothing
        (x:xs) -> Just x

g :: [Bool] -> Bool
g [x, y] = x && not y
g _ = error "g: the input list must contain exactly 2 elements."


main :: IO ()
main = do
    putStrLn "Week 3 exercices: [see below]"

    putStr "g [True, FAlse]: "
    print (g [True, False])

    putStr "isSatisfiable should yield: Just [True,False]: "
    --print (isSatisfiable (BF { numberArgs=2, bfun=g }) )

    print (generateBoolCombinationLists 4)