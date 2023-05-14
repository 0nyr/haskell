module Main where

-- a)
-- In Haskell, you can't place constraints directly on data type 
-- definitions. The main reason is that the constraint doesn't really 
-- make sense at the data level - a value doesn't have a type class, it 
-- simply has a type. The constraint Num a is a requirement on the type a, 
-- not on the values of type a. Instead, you would typically place the 
-- constraint on the functions that operate on the data type. This allows 
-- you to enforce the constraint when the functions are used, not when 
-- the data is created.
data Polynomial a = Coefs [a]
    deriving Show

eval :: Num a => Polynomial a -> a -> a
eval (Coefs []) _ = 0
eval (Coefs listOfCoefs) x = foldr oneMultOneAdd 0 listOfCoefs
    where oneMultOneAdd coef accumulator = coef + (accumulator*x)

-- b)
-- 'replicate' function takes an integer n and a value x, 
-- and returns a list of length n where every element is x
-- here, since zipWith stops at the shotest list, and since
-- in haskell computations are only when necessary,
-- no need to select which list to modify to right length
addPoly :: Num a => Polynomial a -> Polynomial a -> Polynomial a
addPoly (Coefs []) (Coefs []) = Coefs []
addPoly (Coefs coefs) (Coefs []) = Coefs coefs
addPoly (Coefs []) (Coefs coefs) = Coefs coefs
addPoly (Coefs coefs1) (Coefs coefs2) = 
    Coefs (zipWith (+) (coefs1 ++ replicate diffLen 0) (coefs2 ++ replicate diffLen 0))
    where
        maxLen = max (length coefs1) (length coefs2)
        minLen = min (length coefs1) (length coefs2)
        diffLen = maxLen - minLen

-- c)
negatePoly :: Num a => Polynomial a -> Polynomial a
negatePoly (Coefs []) = Coefs []
negatePoly (Coefs listOfCoefs) = Coefs (map (0 - ) listOfCoefs)

-- d)

takeLast :: (Num a) => Int -> [a] -> [a]
takeLast n list = drop ((length list) - n) list


multiplyPoly :: Num a => Polynomial a -> Polynomial a -> Polynomial a
multiplyPoly (Coefs []) (Coefs []) = Coefs []
multiplyPoly (Coefs coefs) (Coefs []) = Coefs coefs
multiplyPoly (Coefs []) (Coefs coefs) = Coefs coefs
multiplyPoly (Coefs coefs1) (Coefs coefs2) = 
    Coefs [ sum (computeCoef i) | i <- [0..((maxLen - 1)*2)]]
    where
        maxLen = max (length coefs1) (length coefs2)
        minLen = min (length coefs1) (length coefs2)
        diffLen = maxLen - minLen
        coefs1goodLen = coefs1 ++ replicate diffLen 0
        coefs2goodLen = coefs2 ++ replicate diffLen 0
        resLen = maxLen*2 - 1
        computeCoef n
            | n == 0 = zipWith (*) (take 1 coefs1goodLen) (take 1 coefs2goodLen)
            | 0 < n && n < maxLen = zipWith (*) (take (n+1) coefs1goodLen) (reverse (take (n+1) coefs2goodLen))
            | maxLen <= n && n < resLen = zipWith (*) (takeLast (resLen - n) coefs1goodLen) (reverse (takeLast (resLen - n) coefs2goodLen))
            | otherwise = error "computeCoef: invalid n"


main :: IO () 
main = do
    putStrLn "Week 4 exercices: [see below]"

    -- a)
    let poly1 = Coefs [3, 2, 0, 5]
    putStr "evalPoly [3, 2, 0, 5] 10 (expect 5023): "
    print (eval poly1 10)

    -- b)
    let poly2 = Coefs [1, 0, 4, 0, 1]
    putStr "addPoly poly1 poly2 (expect [4,2,4,5,1]): "
    print (addPoly poly1 poly2)

    -- c)
    let poly3 = Coefs [2, 0, 3]
    let poly4 = Coefs [5, 0, 6, 9]

    print(takeLast 3 [2, 0, 3, 0])
    print(takeLast 3 [5, 0, 6, 9])

    putStr "multiplyPoly poly3 poly4 (expect [10, 0, 27, 18, 18, 27, 0]): "
    print (multiplyPoly poly3 poly4)
