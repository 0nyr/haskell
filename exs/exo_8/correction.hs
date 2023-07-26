module Homomorphisms where

--
-- A function h :: [a] -> b is called a list homomorphism, if
-- there exists an associative operator (#) :: b -> b -> b
-- with neutral elemente e :: b such that
--
--   h (xs ++ ys) = (h xs) # (h ys)
--   h []         = e
--
-- hold. The idea behind list homomorphisms is that one
-- has a correspondence between the source domain of h
-- (the lists [a] together with the operation (++) and
-- [] as neutral element) and the target domain of h
-- (type b together with the operaton (#) and e as neutral
-- element). One can either compute (with (++)) in lists
-- first and then apply h, or one can apply h first (to xs
-- and ys) and then use (#) in the target domain to get
-- the same result.
--
-- Algebraic structures like ([a],++,[]) and (b,#,e) are 
-- called monoids because they provide an associative
-- operation ((++) and (#), respectively) together with
-- a neutral element ([] and e, respectively). A list
-- homomorphisms are therefore a special case of monoid
-- homomorphisms.
--

--
-- First Homomorphism Law:
--     h is a list homomorphism
-- if and only if
--     there exists a function f, an associative function op
--     and an element e such that e is neutral w.r.t. op and
--       h = foldr op e . map f
--     holds.
--

--
-- If we give a function "f", an associative(!) function "op"
-- and a value "e" which is neutral w.r.t. "op" to "hom", the
-- resulting function is guaranteed to be list homomorphism
-- due to the first homomorphism law.
--
hom :: (a -> b) -> (b -> b -> b) -> b -> [a] -> b
hom f op e = foldr op e . map f

sqrSumH :: Num a => [a] -> a
sqrSumH = hom (^ 2) (+) 0    -- (+) is associative with neutral element 0

concatH :: [[a]] -> [a]
concatH = hom id (++) []     -- (++) is associative with neutral element []

composeH :: [a -> a] -> a -> a
composeH = hom id (.) id     -- (.) is associative with neutral element id

sortH :: Ord a => [a] -> [a]
sortH = hom (\x -> [x]) merge []
    where
        -- Note:
        -- For the implementation it would be sufficient to 
        -- limit ourselves to the cases where the first argument
        -- of merge is just the empty or singleton list. However,
        -- strictly speaking, merge must be an associative
        -- function as stated by the First Homomorphism Law. To
        -- account for this, our definition of merge can also
        -- take lists with more than one element as its first
        -- argument, even though these cases never occur in the
        -- context of sortH.
        merge :: Ord a => [a] -> [a] -> [a]

        {-
        -- Non-associative implementation of merge that only
        -- works with singleton lists as first argument.
        -- Sufficient for a correct implementation of sortH
        -- where all possibly occurring patterns are covered.
        merge [x] []     = [x]
        merge [x] (y:ys)
            | x < y     = x : y : ys
            | otherwise = y : merge [x] ys
        -}

        -- Implementation of merge that works with input
        -- lists of arbitrary sizes. Necessary for
        -- associativity as demanded by the First Homomorphism
        -- Law.
        merge xs []     = xs
        merge [] ys     = ys
        merge (x:xs) (y:ys)
            | x < y     = x : merge xs (y:ys)
            | otherwise = y : merge (x:xs) ys

--
-- Is the function
--   average :: Fractional a => [a] -> a
-- a list homomorphism?
--
-- Claim: average is not a list homomorphism.
-- Proof by contradiction:
--   Assume that function "average" is a list homomorphism.
--   Then (by definition of a list homomorphism), there exists
--   an associative operator
--      (#) :: a -> a -> a
--   and an element
--      e :: a
--   such that e is neutral w.r.t. (#) and
--      i) average [] == e
--     ii) average (xs ++ ys) = average xs # average ys 
--   hold.
--   For "average", i) is clearly violated because  average []
--   is not defined (i.e., it cannot be a value which is
--   neutral w.r.t. (#)). Therefore, the assumption is wrong
--   and "average" is not a list homomorphism.
--
--   We can also use ii) to show that the assumption is wrong:
--       2
--   ==  {- definition of average -}
--       average [1,1,4]
--   ==  {- ii) -}
--       average [1,1] # average [4]
--   ==  {- definition of average -}
--       1 # 4
--   ==  {- definition of average -}
--       average [1] # averag [4,4]
--   ==  {- ii) -}
--       average [1,4,4]
--   ==  {- definition of average -}
--       3
--
--   We derive the contradiction 2 == 3 from the assumption that
--   "average" is a list homomorphisms; therefore, this assumption
--   is wrong and "average" is not a list homomorphism.
-- q.e.d.
--

--
-- Is the following function a list homomorphism?
--
rotate :: [a] -> [a]
rotate []     = []
rotate (x:xs) = xs ++ [x]

--
-- Claim: "rotate" is a list homomorphism.
-- Proof sketch:
-- Since  rotate [] == [],  [] be the neutral element in the target monoid.
-- The following operator (#)
--
(#) [] ys = ys
(#) xs [] = xs
(#) xs ys = init xs ++ [last ys] ++ init ys ++ [last xs]
--
-- is associative, has neutral element [] (= rotate []) and satisfies
-- the homomorphism definition:
--    rotate (xs ++ ys) = rotate xs # rotate ys      (*)
-- (To write a complete prove, one would need to prove that (#) is associative,
-- that [] is neutral w.r.t. (#) and that equation (*) really holds.)
-- 
-- To see intuitively that (#) works as expected, consider the following example:
--   rotate [1..10]  == [2,3,4,5,6,7,8,9,10,1]
--   rotate [1..4]   == [2,3,4,1]
--   rotate [5..10]  == [6,7,8,9,10,5]
-- To obtain  rotate [1..10]  from  rotate [1..4] and rotate [5..10],
-- the results of  rotate [1..4] and rotate [5..10]  have to be concatenated
-- after exchanging their last values.
--
