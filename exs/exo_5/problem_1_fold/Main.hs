module Main where

import Data.List

myAssert :: Bool -> IO ()
myAssert True = putStrLn "🟢 assertion ok"
myAssert False = putStrLn "🔴 assertion failed"

-- a)
fromTo :: (Num a, Ord a) => a -> a -> [a]
fromTo x y = unfoldr f x
    where
        f a
            | a <= y = Just (a, a+1)
            | otherwise = Nothing

-- b)
mymap :: (t -> a) -> [t] -> [a]
mymap f list = unfoldr internalF list
    where 
        internalF [] = Nothing
        internalF (x:xs) = Just (f x, xs)


fTest :: (Ord a, Num a) => a -> Bool
fTest x = x > 5

fTest2 :: (Fractional a, Ord a) => a -> a
fTest2 x = x^3 + 1 + 3/(max 10 x)

-- c)
gTestUnfold :: Num b => b -> Maybe (a, b)
gTestUnfold x 
    | res < 100 = Just (res, res*res)
    | otherwise = Nothing
    where
        res = (x*3 + 3)

fTestFold :: (Num a, Num b) => a -> b -> a
fTestFold a b = a + b - 1

-- foldUnfoldr :: Foldable t => (a -> b -> b) -> b -> (b -> Maybe (a, b)) -> b -> a
-- foldUnfoldr f x g y =


main :: IO ()
main = do
    putStrLn "Week 5 exercices: [see below]"


    -- a)
    myAssert (fromTo 5 10 == [5..10])

    -- b)
    myAssert ((mymap fTest [1,3..10]) == (map fTest [1,3..10]))
    myAssert ((mymap fTest2 [1,3..10]) == (map fTest2 [1,3..10]))

    -- c)
    let x = 0
    let y = 0
    let f = fTestFold
    let g = gTestUnfold
    
    putStrLn "unfoldr gTestUnfold 0: "
    print (unfoldr gTestUnfold 0)
    putStrLn "foldr fTestFold 0 (unfoldr gTestUnfold 0): "
    print (foldr fTestFold 0 (unfoldr gTestUnfold 0))

    
    myAssert (foldUnfoldr f x g y == foldr f x (unfoldr g x))