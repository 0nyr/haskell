module Main where

-- reduceL (+) 0 [1,2,3] -> ((((0)+1)+2)+3)
reduceL :: (o -> i -> o) -> o -> [i] -> o
reduceL f x [] = x
reduceL f x (y:restOfList) = 
    reduceL f (f x y) restOfList 

reduceL2 :: (o -> i -> o) -> o -> [i] -> o
reduceL2 f x [] = x
reduceL2 f x (y:restOfList) = 
    f (reduceL2 f x restOfList) y

-- reduceR (+) 0 [1,2,3] -> (1+(2+(3+(0))))
reduceR :: (i -> o -> o) -> o -> [i] -> o
reduceR f x [] = x
reduceR f x [y] = f y x
reduceR f x (y:restOfList) = 
    reduceR f (f y x) restOfList

reduceR2 :: (i -> o -> o) -> o -> [i] -> o
reduceR2 f x [] = x
reduceR2 f x [y] = f y x
reduceR2 f x (y:restOfList) = 
    f y (reduceR2 f x restOfList)


main :: IO ()
main = do
    putStrLn "Week 4 exercices: [see below]"

    putStr "reduceL (+) 0 [1,2,3] (expect 6): "
    print (reduceL (+) 0 [1,2,3])

    -- (((0-1)-2)-3)
    putStr "reduceL2 (-) 0 [1,2,3] (expect -6): "
    print (reduceL2 (-) 0 [1,2,3])

    -- (1-(2-(3-0)))
    putStr "reduceR (-) 0 [1,2,3] (expect 2): "
    print (reduceR (-) 0 [1,2,3])

    putStr "reduceR2 (-) 0 [1,2,3] (expect 2): "
    print (reduceR2 (-) 0 [1,2,3])