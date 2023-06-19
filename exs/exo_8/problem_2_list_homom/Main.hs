module Main where

hom :: (a->b) -> (b->b->b) -> b -> [a] -> b
hom f op e = foldr op e . map f

average :: [Double] -> Double
average list = hom (/ fromIntegral (length list)) (+) 0 list


rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]
-- Is rotate a list homomorphism?
-- rotate (xs ++ ys) = rotate xs # rotate ys
-- rotate [] = []

-- rotate [1,2,3] = rotate [1,2] # rotate [3]
-- rotate [1,2,3] = [2,1] # [3]

[]

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
    print (average [1,2,3]*2)
    print (average [2,4,6])
    myAssert (allEqual [(average [1,2,3])*2, average [2,4,6]])