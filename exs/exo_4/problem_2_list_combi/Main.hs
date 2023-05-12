module Main where

-- a)
sumAndLenCounter :: (Num a, Num b) => (a, b) -> a -> (a, b)
sumAndLenCounter (a,counter) b = (a + b, counter + 1)


computeSumAndLength :: Fractional a => [a] -> a
computeSumAndLength [] = error "computeSumAndLength: empty list"
computeSumAndLength [x] = x
computeSumAndLength list = 
    let (totalSum, len) = foldl sumAndLenCounter (0, 0) list
    in totalSum / len


main :: IO ()
main = do
    putStrLn "Week 4 exercices: [see below]"

    -- a)
    putStr "computeSumAndLength [1,2,3] (expect 2): "
    print (computeSumAndLength [1,2,3])