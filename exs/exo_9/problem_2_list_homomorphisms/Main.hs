module Main where

-- Are the following functions list homomorphisms? 
-- Give either a proof sketch (if the respective
-- function is a list homomorphism) or a counter example 
-- (if it is not).

-- a) Maximum segment sum
mss :: [Integer] -> Integer
mss = maximum . map sum . segments
    where
      segments :: [a] -> [[a]]
      segments [] = []
      segments list = concat . map inits . tails list

-- Is mss a list homomorphism?
-- mss (xs ++ ys) = mss xs ? mss ys
-- mss [] = 0 
-- so has a neutral element
-- we can rewrite mss as:
-- mss = maximum . map sum . segments
-- mss = foldr max 0 . map sum . segments
-- so mss is a list homomorphism

-- b)
e :: a -- neutral element
e = undefined

g :: a -> a -> a -- associative
g = undefined

s :: [a] -> [a]
s = scanl g e

-- Is s a list homomorphism?
-- s (xs ++ ys) = s xs ? s ys
-- s (xs ++ ys) = scanl g e xs ? scanl g e ys
-- s ([1,2] ++ [3,4]) = scanl g e [1,2] ? scanl g e [3,4]
-- s ([1,2] ++ [3,4]) = [e, g e 1, g (g e 1) 2] ? [e, g e 3, g (g e 3) 4]

-- ? = xs ++ tail (scan g (last xs) ys)

-- s [1,2,3,4] = [e, g e 1, g (g e 1) 2, g (g (g e 1) 2) 3, g (g (g (g e 1) 2) 3) 4]
-- so s is a list homomorphism

main :: IO ()
main = do
    putStrLn "Week 9 exercices: [see below]"