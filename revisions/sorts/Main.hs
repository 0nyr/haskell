module Main where

-- insertion sort
insertsort :: Ord a => [a] -> [a]
insertsort [] = []
insertsort (x:xs) = insertElement x (insertsort xs)
    where 
        insertElement x [] = [x]
        insertElement x (headElement:tailList) = if x > headElement 
                                                    then headElement : insertElement x tailList
                                                    else x : headElement : tailList

-- whatever the direction of element taken, the list order is the same
-- the only difference between foldr and foldl is in which order the element of the list will be considered.
insertSort2 :: Ord a => [a] -> [a]
insertSort2 = foldr insert []
    where
    insert x [] = [x]
    insert x (y:ys)
        | x <= y    = x : y : ys
        | otherwise = y : insert x ys

insertSort3 :: Ord a => [a] -> [a]
insertSort3 = foldl (flip insert) []
    where
    insert x [] = [x]
    insert x (y:ys)
        | x <= y    = x : y : ys
        | otherwise = y : insert x ys



-- quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =  quicksort (filter (< x) xs)
                     ++ [x] ++ 
                    quicksort (filter (>= x) xs)

-- merge sort from top (list) to bottom (singleton)
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf) -- create singletons and merge just after
    where
        half = length xs `div` 2
        (firstHalf, secondHalf) = splitAt half xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- merge sort from bottom (singleton) to top (list)
mergeSort' :: Ord a => [a] -> [a]
mergeSort' xs = mergeAll (map (\x -> [x]) xs)

mergeAll :: Ord a => [[a]] -> [a]
mergeAll [x] = x
mergeAll xs = mergeAll (mergePairs xs)

mergePairs :: Ord a => [[a]] -> [[a]]
mergePairs (x:y:xs) = merge x y : mergePairs xs
mergePairs xs = xs

-- bubble sort
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = bubbleSort (init xs') ++ [last xs']
    where xs' = bubble xs

bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs)
    | x <= y    = x : bubble (y:xs)
    | otherwise = y : bubble (x:xs)




-- Tests
unsortedList = [1, 5, 2, 4, 3, -1, 8, 7]
sortedList = [-1, 1, 2, 3, 4, 5, 7, 8]

myAssert :: Bool -> IO ()
myAssert True = putStrLn "🟢 assertion ok"
myAssert False = putStrLn "🔴 assertion failed"

-- Check if all elements in a list are equal
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

main :: IO ()
main = do
    putStrLn "Revisions around sorts: [see below]"

    -- insert sort
    myAssert (allEqual [insertsort unsortedList, sortedList])
    myAssert (allEqual [insertSort2 unsortedList, sortedList])
    myAssert (allEqual [insertSort3 unsortedList, sortedList])

    -- quicksort
    myAssert (allEqual [quicksort unsortedList, sortedList])

    -- merge sort
    myAssert (allEqual [mergeSort unsortedList, sortedList])
    myAssert (allEqual [mergeSort' unsortedList, sortedList])

    -- bubble sort
    myAssert (allEqual [bubbleSort unsortedList, sortedList])