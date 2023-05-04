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

-- Q: c)
genList :: (Integer->a) -> Integer -> [a]
genList f 0 = [f 0]
genList f n = f n : genList f (n - 1)

genListGoodOrder :: (Integer->a) -> Integer -> [a]
genListGoodOrder f n = reverse (genList f n)

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
