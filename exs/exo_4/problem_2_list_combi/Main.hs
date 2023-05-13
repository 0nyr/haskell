module Main where
import Data.Char
import Data.List

-- a)
sumAndLenCounter :: (Num a, Num b) => (a, b) -> a -> (a, b)
sumAndLenCounter (a,counter) b = (a + b, counter + 1)


computeSumAndLength :: Fractional a => [a] -> a
computeSumAndLength [] = error "computeSumAndLength: empty list"
computeSumAndLength [x] = x
computeSumAndLength list = 
    let (totalSum, len) = foldl sumAndLenCounter (0, 0) list
    in totalSum / len

-- b)
upLetterIfNeededAndLenCounter :: (Num a) => a -> Char -> (a, Char)
upLetterIfNeededAndLenCounter counter charVal
    | isLower charVal = (counter + 1, toUpper charVal)
    | otherwise = (counter + 1, charVal)

-- using mapAccumL :: (a->x->(a,y)) -> a -> [x] -> (a,[y])
makeUpper :: String -> (Int, String)
makeUpper listOfChar = 
    (len, resList)
    where (len, resList) = mapAccumL upLetterIfNeededAndLenCounter 0 listOfChar


main :: IO ()
main = do
    putStrLn "Week 4 exercices: [see below]"

    -- a)
    putStr "computeSumAndLength [1,2,3] (expect 2): "
    print (computeSumAndLength [1,2,3])

    -- b)
    putStr "makeUpper 'Hello world' (expect 2): "
    print (makeUpper "Hello world")