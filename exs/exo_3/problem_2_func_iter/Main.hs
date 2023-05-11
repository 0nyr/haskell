module Main where

import Test.HUnit

nTimes :: (Num a, Num b, Eq b) => b -> (a -> a) -> (a -> a)
nTimes n f
    | lastIter = f
    | not lastIter = nTimes (n - 1) f.f -- composition of f (call f on f)
    -- stop to 1, otherwise, one time too much:
    -- ex on nTimes 5 F: 5 F -> 4 FF -> 3 FFF -> 2 FFFF -> 1 FFFFF -> 0 FFFFFF -> FFFFFF
    where lastIter = n == 1

nTimesCorrection :: Int -> (a -> a) -> (a -> a)
--nTimesCorrection :: Int -> ((a, a) -> (a, a)) -- similar, think about partially applied functions
nTimesCorrection 0 _ = id -- identity function
--nTimesCorrection 1 f = f      -- not necessary
--nTimesCorrection 2 f = f.f    -- not necessary   
nTimesCorrection n f
    | n >= 0 = f . nTimesCorrection (n - 1) f -- WARN: nTimesCorrection (n - 1) (f.f) is wrong !!!
    | otherwise = error "nTimesCorrection: n must not be negative."
-- understand that  "nTimes (n - 1) f.f" is actually same as  "(nTimes (n - 1) f).f" !!!

nTimesCorrection2 :: Int -> (a -> a) -> (a -> a)
nTimesCorrection2 n f
    | n >= 0 = nT n -- nT is a helper functior for recursion
    | otherwise = error "nTimesCorrection2: n must not be negative."
    where
        -- no need to put f as input !!!
        nT 0 = id
        nT k = f . nT (k - 1)

mul2 :: Num a => a -> a
mul2 x = x * 2

mul32 :: Num a => a -> a
mul32 = nTimes 5 mul2


-- Test cases
test1 = TestCase (assertEqual "Test case 1" (mul32 1) 32)
test2 = TestCase (assertEqual "Test case 2" (mul32 2) 64)
test3 = TestCase (assertEqual "Test case 3" (mul32 3) 96)
test4 = TestCase (assertEqual "Test case 4" (nTimes 3 mul2 1) 8)
test5 = TestCase (assertEqual "Test case 5" (nTimes 4 mul2 1) 16)

tests = TestList [test1, test2, test3, test4, test5]

main :: IO ()
main = do
    putStrLn "Week 3 exercices: [see below]"

    -- ascendingSort2
    putStr "ascendingSort2 1 2: "
    print (mul32 2)

    -- run the test cases and print the results
    putStrLn "\n-- Test results --"
    testResults <- runTestTT tests
    print testResults