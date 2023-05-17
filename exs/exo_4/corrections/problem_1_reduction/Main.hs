module Main where

-- CORRECT
-- reduceL (+) 0 [1,2,3] -> ((((0)+1)+2)+3)
reduceL :: (o -> i -> o) -> o -> [i] -> o
reduceL f x [] = x
reduceL f x (y:restOfList) = 
    reduceL f (f x y) restOfList 

-- this variant reverses the list:
--  reduceL2 (+) 0 [1,2,3] = ((0 + 3) + 2) + 1
reduceL2 :: (o -> i -> o) -> o -> [i] -> o
reduceL2 f x [] = x
reduceL2 f x (y:restOfList) = 
    f (reduceL2 f x restOfList) y

-- reduceR (+) 0 [1,2,3] -> (1+(2+(3+(0))))

-- This definition reverses the list: (WRONG)
--  reduceR (+) 0 [1,2,3] = 3+(2+(1+0))
-- (The case for [y] could be removed.)
reduceR :: (i -> o -> o) -> o -> [i] -> o
reduceR f x [] = x
reduceR f x [y] = f y x
reduceR f x (y:restOfList) = 
    reduceR f (f y x) restOfList

-- This variant does not reverse the list, i.e.,
--  reduceR2 (+) 0 [1,2,3] = 1 + (2 + (3+0))
-- (as expected). (The case for [y] could be removed.)
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

-- Test works because it actually computes ((0-3)-2)-1 which gives the same result
    -- (((0-1)-2)-3)
    putStr "reduceL2 (-) 0 [1,2,3] (expect -6): "
    print (reduceL2 (-) 0 [1,2,3])

-- By chance (or: bad luck in this case...), the computatiot 3-(2-(1-0)) in reduceR yields the same result.
-- Try with [1,2,3,4] to get different results from reduceR and redureR2.
    -- (1-(2-(3-0)))
    putStr "reduceR (-) 0 [1,2,3] (expect 2): "
    print (reduceR (-) 0 [1,2,3])

    putStr "reduceR2 (-) 0 [1,2,3] (expect 2): "
    print (reduceR2 (-) 0 [1,2,3])
