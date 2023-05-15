module Main where

import Debug.Trace

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

-- debugging version
multiplyPolyShow :: (Num a, Show a) => Polynomial a -> Polynomial a -> Polynomial a
multiplyPolyShow (Coefs []) (Coefs []) = Coefs []
multiplyPolyShow (Coefs coefs) (Coefs []) = Coefs coefs
multiplyPolyShow (Coefs []) (Coefs coefs) = Coefs coefs
multiplyPolyShow (Coefs coefs1) (Coefs coefs2) =
    Coefs [ sum (computeCoef i) | i <- [0..((maxLen - 1)*2)]]
    where
        maxLen = max (length coefs1) (length coefs2)
        minLen = min (length coefs1) (length coefs2)
        diffLen = maxLen - minLen
        -- WARN: It is MANDATORY that the two lists used for computing are of the same size !!!
        -- otherwise, the 'take' operations will fail !!!
        coefs1goodLen = if length coefs1 < maxLen then coefs1 ++ replicate diffLen 0 else coefs1
        coefs2goodLen = if length coefs2 < maxLen then coefs2 ++ replicate diffLen 0 else coefs2
        resLen = maxLen*2 - 1
        computeCoef n
            | n == 0 = zipWith (*) (take 1 coefs1goodLen) (take 1 coefs2goodLen)
            | 0 < n && n < maxLen = zipWith (*) (take (n+1) coefs1goodLen) (reverse (take (n+1) coefs2goodLen))
            | maxLen <= n && n < resLen = trace ("(resLen - n) value: " ++ show (resLen - n)) $ trace ("coefs1goodLen value: " ++ show (coefs1goodLen)) $ trace ("coefs2goodLen value: " ++ show (coefs2goodLen)) $ trace ("zipwith value: " ++ show ((zipWith (*) (takeLast (resLen - n) coefs1goodLen) (take (resLen - n) (reverse coefs2goodLen))))) $ zipWith (*) (takeLast (resLen - n) coefs1goodLen) (take (resLen - n) (reverse coefs2goodLen))
            -- | maxLen <= n && n < resLen = zipWith (*) (takeLast (resLen - n) coefs1goodLen) (take (resLen - n) (reverse coefs2goodLen))
            | otherwise = error "computeCoef: invalid n"

-- more elegant version
multiplyPoly :: Num a => Polynomial a -> Polynomial a -> Polynomial a
multiplyPoly (Coefs coefs1) (Coefs coefs2) =
    Coefs [ sum (computeCoef i) | i <- [0..((maxLen - 1)*2)]]
    where
        maxLen = max (length coefs1) (length coefs2)
        minLen = min (length coefs1) (length coefs2)
        diffLen = maxLen - minLen
        resLen = maxLen*2 - 1

        coefs1goodLen = extendWithZeros coefs1 maxLen
        coefs2goodLen = extendWithZeros coefs2 maxLen

        extendWithZeros coefs len
            | length coefs < len = coefs ++ replicate diffLen 0
            | otherwise          = coefs

        computeCoef n
            | n < maxLen = zipWith (*) (take (n+1) coefs1goodLen) (reverse (take (n+1) coefs2goodLen))
            | otherwise  = zipWith (*) (takeLast (resLen - n) coefs1goodLen) (take (resLen - n) (reverse coefs2goodLen))


-- e)
createConstPoly :: a -> Polynomial a
createConstPoly constant = Coefs [constant]

-- f)
createPolyX :: Num a => Polynomial a
createPolyX = Coefs [0, 1] -- x has coefs 0 + 1.x -> [0, 1]

-- 'x' is a (polymorphic) constant x representing the polynomial X
x :: Num a => Polynomial a
x = createPolyX

-- g)
-- If we were in Rust: we specify implementations of the trait Num to the class Polynomial
-- In Haskell: we specify that Polynomial a (where a has the class of Num) is an instance of the class Num (i.e. behave as a Num)
-- NOTE: (-) works automatically when 'negate' is defined
instance (Num a) => Num (Polynomial a) where
    (+), (*)   :: Polynomial a -> Polynomial a -> Polynomial a
    negate  :: Polynomial a -> Polynomial a
    fromInteger :: Integer -> Polynomial a
    -- default definitions
    (+) = addPoly
    (*) = multiplyPoly
    negate = negatePoly
    fromInteger x = createConstPoly (fromInteger x) -- need to convert x to Num here
    abs = undefined
    signum = undefined



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
    putStr "negatePoly poly1 (expect [-3, -2, 0, -5]): "
    print (negatePoly poly1)

    -- d)
    let poly3 = Coefs [2, 0, 3]
    let poly4 = Coefs [5, 0, 6, 9]

    print (takeLast 3 [2, 0, 3, 0])
    -- print(takeLast 3 (reverse [5, 0, 6, 9]))
    print (take 3 (reverse [1, 2, 3, 4]))
    print (take (3) (reverse [5, 0, 6, 9]))

    let (Coefs list3, Coefs list4) = (poly3, poly4)
        maxLenTest = max (length list3) (length list4)
        resLenTest = maxLenTest*2 - 1

    print (zipWith (*) (takeLast (3) [2, 0, 3, 0]) (take (3) (reverse [5, 0, 6, 9])))

    putStr "multiplyPoly poly3 poly4 (expect [10, 0, 27, 18, 18, 27, 0]): "
    print (multiplyPoly poly3 poly4)

    -- e)
    putStr "createConstPoly 10 (expect [10]): "
    print (createConstPoly 10)

    -- f)
    putStr "createConstPoly 10 (expect [0, 1]): "
    print createPolyX
    print x

    -- g)
    putStr "(3*x-x*(5-x))*(x*x-x) (expect: [0,0,2,-3,1]): "
    print ((3*x-x*(5-x))*(x*x-x))
