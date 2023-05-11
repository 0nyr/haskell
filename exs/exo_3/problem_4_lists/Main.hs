module Main where

-- Q: a)
gcdList :: Integral a => [a] -> a
gcdList [] = error "gcdList: input list cannot be empty"
gcdList [last] = last -- gcd of 1 element is the element 
gcdList (a:tail) = gcd a (gcdList tail)

-- Q: b)
facs :: Integer -> [Integer]
facs 0 = [1]
facs n = 
    let lastFacs = facs (n - 1)
    in
        ( head lastFacs ) * n : lastFacs

facsInGoodOrder :: Integer -> [Integer]
facsInGoodOrder n = reverse (facs n) -- reverse is the efficient way of reversing list


facsCorrection :: Integer -> [Integer]
facsCorrection 0 = [1]
facsCorrection n = facs (n - 1) ++ [ factorial n ]
    where
        factorial 0 = 1
        factorial k = k * factorial (k - 1)

facsCorrectionNoRecursion :: Integer -> [Integer]
facsCorrectionNoRecursion 0 = [1]
facsCorrectionNoRecursion n = [ factorial i | i <- [0..n]]
    where   
        factorial 0 = 1
        factorial k = k * factorial (k - 1)

facsCorrectionMap :: Integer -> [Integer]
facsCorrectionMap 0 = [1]
facsCorrectionMap n =
    map factorial [0..n]
    where   
        factorial 0 = 1
        factorial k = k * factorial (k - 1)

-- Q: c)
genList :: (Integer->a) -> Integer -> [a]
genList f 0 = [f 0]
genList f n = f n : genList f (n - 1)

genListGoodOrder :: (Integer->a) -> Integer -> [a]
genListGoodOrder f n = reverse (genList f n)

genListCorrection :: (Integer->a) -> Integer -> [a]
genListCorrection f n = map f [0..n-1]

genListCorrection2 :: (Integer->a) -> Integer -> [a]
genListCorrection2 f n = [f i | i <- [0..n]]


-- Q: d)
iterList :: (a -> a) -> a -> Integer -> [a]
iterList f x 0 = [x]
iterList f x n =
    let lastIter = iterList f x (n - 1)
    in 
         f (head lastIter) : lastIter

iterListGoodOrder :: (a -> a) -> a -> Integer -> [a]
iterListGoodOrder f x n = reverse (iterList f x n)

-- Q: e)
isPrime :: Integer -> Bool
isPrime 1 = False -- 1 is not a prime number
isPrime n = 
    let computeRemainder n d = n `mod` d
    in
        length [ divider | divider <- [1 .. round (sqrt (fromIntegral n :: Float))], computeRemainder n divider == 0] == 1

isPrimeCorrection :: Integer -> Bool
isPrimeCorrection n = n >=2 && null [d | d <- twoToSqrtN, mod n d == 0]
    where
        -- bad: twoToSqrtN = floor (sqrt (fromIntgral n :: Float))
        -- a bit better: twoToSqrtN = [i | i <- [2..n - 1], i*i <= n]
        twoToSqrtN = takeWhile (\i -> i*i <= n) [2..n - 1] -- best: using takewhile to stop the list after the condition is false

main :: IO ()
main = do
    putStrLn "Week 3 exercices: [see below]"

    putStr "gcdList: [160,152,-8000,0] == 8: "
    print (gcdList [160,152,-8000,0])

    putStr "(facsInGoodOrder 4), last is 24: "
    print (facsInGoodOrder 4)

    putStr "genListGoodOrder (+ 1) 5:"
    print (genListGoodOrder (+ 1) 5)

    putStr "iterListGoodOrder"
    print (iterListGoodOrder (^2) 3 5)
