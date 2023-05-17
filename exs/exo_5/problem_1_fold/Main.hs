module Main where

import Data.List
import Debug.Trace

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
gTestUnfoldShow :: (Num a, Ord a, Show a) => a -> Maybe (a, a)
gTestUnfoldShow x 
    | res < 1000 = trace ("x: " ++ show (x)) $ trace ("res: " ++ show (res)) $ Just (res, res)
    | otherwise = Nothing
    where
        res = (x*2 + 3)

gTestUnfold :: (Num a, Ord a) => a -> Maybe (a, a)
gTestUnfold x 
    | res < 1000 = Just (res, res)
    | otherwise = Nothing
    where
        res = (x*2 + 3)

fTestFold :: Num a => a -> a -> a
fTestFold a b = a + b - 1

foldUnfoldr :: (a -> b -> b) -> b -> (b -> Maybe (a, b)) -> b -> b
foldUnfoldr f x g y =
    case maybe_g_res_tuple of
        Just (resA, resB) -> foldUnfoldr f (f resA x) g resB
        Nothing -> x
    where maybe_g_res_tuple = g y


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
    print (unfoldr g x)
    putStrLn "foldr fTestFold 0 (unfoldr gTestUnfold 0): "
    print (foldr f y (unfoldr g x))
    -- check in python: sum([3,9,21,45,93,189,381,765]) - len([3,9,21,45,93,189,381,765]) == 1498

    myAssert (foldUnfoldr f x g y == foldr f x (unfoldr g x))