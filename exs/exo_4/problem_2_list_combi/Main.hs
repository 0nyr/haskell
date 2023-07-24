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

-- correction
averageCorrection :: Fractional a => [a] -> a
averageCorrection [] = error "average undefined for empty list"
averageCorrection xs =
    let (totalSum, totalLength) = foldl f (0,0) xs
    in totalSum / fromIntegral totalLength
    where
        f (s, counter) x = (s + x, counter + 1)

-- b)
upLetterIfNeededAndLenCounter :: (Num a) => a -> Char -> (a, Char)
upLetterIfNeededAndLenCounter counter charVal
    | isLower charVal = (counter + 1, toUpper charVal)
    | otherwise = (counter, charVal)

-- using mapAccumL :: (a->x->(a,y)) -> a -> [x] -> (a,[y])
makeUpper :: String -> (Int, String)
makeUpper listOfChar = 
    (len, resList)
    where (len, resList) = mapAccumL upLetterIfNeededAndLenCounter 0 listOfChar

-- corrections
-- bad opti, reads 3 times the list (length, filter, map)
makeUpperSimpleCorrection :: String -> (Int, String)
makeUpperSimpleCorrection str = (length(filter isLower str), map toUpper str)

makeUpperCorrectionOpti :: String -> (Int, String)
--makeUpperCorrectionOpti str = foldr f (0, "") str
makeUpperCorrectionOpti = foldr f (0, "") -- arg 'str' is no necessary
    where
        f c (count, s) = if isLower c then (count + 1, toUpper c : s)
            else (count, c : s)

-- c)
evalPoly :: Num a => [a] -> a -> a
evalPoly [] _ = 0
evalPoly list x = foldr oneMultOneAdd 0 list
    where oneMultOneAdd coef accumulator = coef + (accumulator*x)

-- d)
-- remove first element of the list (derivation of constant)
-- second element is conserved (derive x)
-- third element multiplied by 2 (derive x^2)
-- fourth element multiplied by 3 (derive x^3)
-- etc...
derivePoly :: Num a => [a] -> [a]
derivePoly [] = []
derivePoly [constant] = []
derivePoly (_:listOfCoefs) = 
    zipWith (*) listOfCoefs arithmeticSeq
    where arithmeticSeq = map fromIntegral [1 .. length listOfCoefs]

-- WARN: cannot replace fmult with (*) since (*) needs inputs to be of same type... 
--  and operator .. is not available for all Num.
derivePolyCorrection :: Num a => [a] -> [a]
derivePolyCorrection [] = []
derivePolyCorrection (_:coeffs) = zipWith fmult coeffs [1..length coeffs]
    where 
        fmult a b = a * fromIntegral b

main :: IO ()
main = do
    putStrLn "Week 4 exercices: [see below]"

    -- a)
    putStr "computeSumAndLength [1,2,3] (expect 2): "
    print (computeSumAndLength [1,2,3])

    -- b)
    putStr "makeUpper 'Hello world' (expect 2): "
    print (makeUpper "Hello world")

    -- c)
    putStr "evalPoly [3, 2, 0, 5] 10 (expect 5023): "
    print (evalPoly [3, 2, 0, 5] 10)

    -- d)
    putStr "derivePoly [3, 2, 0, 5] (expect [2,0,15]): "
    print(derivePoly [3, 2, 0, 5])

