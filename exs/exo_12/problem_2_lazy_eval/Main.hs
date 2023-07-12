module Main where

-- tips: forget all you think you know about programming
-- and embrace the laziness of haskell
-- here, we define infinite lists, and we can use them
-- exe: roundrobin [1,2,3] !! (10^6)

-- a)
-- [0,2,4,6,8,10,12,...] = 0 : map (+2) [0,2,4,6,8,10,12,...]
evenNumbers :: [Integer]
evenNumbers = 0 : map (+2) evenNumbers

-- b)
-- fibs =       [0,1,1,2,3,5,8,13,21,...]
-- tail fibs =  [1,1,2,3,5,8,13,21,34,...]
-- e by e 0: 1: [1,2,3,5,8,13,21,34,55,...]
fibsCorrection = 0: 1: zipWith (+) fibsCorrection (tail fibsCorrection)

fibs :: [Integer]
fibs = 0 : 1 : applyFibN fibs
    where
        applyFibN :: [Integer] -> [Integer]
        applyFibN (x:y:xs) = (x + y) : applyFibN (y:xs)

-- c) Sieve of Eratosthenes


primes :: [Integer]
primes = sieve [2..]
    where
        sieve :: [Integer] -> [Integer]
        sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

-- d) Round robin, naive implementation
roundrobin :: [a] -> [a]
roundrobin xs = xs ++ roundrobin xs

-- e) Round robin, better implementation
roundrobinFixedMem :: [a] -> [a]
roundrobinFixedMem = cycle



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
    putStr "take 10 evenNumbers:"
    print (take 10 evenNumbers)

    -- b)
    putStr "take 10 fibs:"
    print (take 10 fibs)

