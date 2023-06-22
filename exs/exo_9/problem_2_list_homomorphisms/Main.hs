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
-- CORRECTION NOTE: THE FOLLOWING IS WRONG
-- mss (xs ++ ys) = mss xs ? mss ys
-- mss [] = 0 
-- so has a neutral element
-- we can rewrite mss as:
-- mss = maximum . map sum . segments
-- mss = foldr max 0 . map sum . segments
-- so mss is a list homomorphism

-- intuition that mss is NOT a list homomorphism
-- if you have a list and you cut it in 2, 
-- then apply the mss function on each part
-- we don't get the same result as if we apply mss on the whole list
--
-- Proposition of proof:
-- Proof by contradiction: Assume that mss is a list homomorphism
-- Then, there exists an associative operator # with 
-- a neutral element e such that:
--    mss (xs ++ ys) = mss xs # mss ys
--    mss [] = e
--
-- 4
-- = mss [3, -2, 3] -- 3 + (-2) + 4 = 4: here the maximum segment sum is 4 considering the whole list
-- mss [3] # mss [-2, 3]
-- = 3 # 3
-- = mss [3] # mss[3]
-- = mss [3,3]
-- = 6
-- We can see a contradiction here
-- So it means mss is not an homomorphism
--

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

-- ? = xs ++ tail (scanl g (last xs) ys) WRONG
-- ? = xs ++ map(\y -> last xs `g` y) (tail ys) CORRECTED VERSION
--
-- neutral element e = provided by definition

-- s [1,2,3,4] = [e, g e 1, g (g e 1) 2, g (g (g e 1) 2) 3, g (g (g (g e 1) 2) 3) 4]
-- so s is a list homomorphism


-- correction
--



main :: IO ()
main = do
    putStrLn "Week 9 exercices: [see below]"
