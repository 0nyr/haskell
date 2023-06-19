--
-- The pragma {-# ... #-} sets options for GHC:
-- turn on optimization and output statistics
-- about the simplifier's work.
--
-- The simplifier statistics contain the names of
-- the rules applied, i.e., one can check whether
-- a rule has been used.
--
-- Compile this file with:
--   ghc -o rulesdemo RulesDemo.hs
--
-- (The module has to be named "Main" or one would have
-- to use the option "-main-is" when compiling,
-- for example, "-main-is RulesDemo.main"
-- if the module were named "RulesDemo".)
--

{-# OPTIONS_GHC -O -ddump-simpl-stats #-}
module Main where

--
-- Our own list data type (to keep the
-- simplifier from applying the optimizatios
-- defined on the built-in lists).
--
data List a = Nil
            | Cons a (List a)
              deriving (Eq, Show)

--
-- Create a list with integers [a..b].
--
fromTo :: Integer -> Integer -> List Integer
fromTo a b
    | a > b     = Nil
    | otherwise = Cons a (fromTo (a+1) b)

--
-- Sum up the values in a list.
-- (The helper function 'h' is tail
-- recursive to avoid stack overflows
-- with long lists.)
--
addup :: List Integer -> Integer
addup = h 0
    where
      h acc Nil         = acc
      h acc (Cons x xs) = h (acc+x) xs

--
-- Optimization rule for expression "addup (fromTo x y)":
-- Use the well-knows summation formula to short-cut
-- the computation.
--
-- Rules are evaluted from left to right, i.e.,
-- "addup (fromTo x y)" can be replaced by "if y >= x ..."
-- but not the other way round.
--
-- Note: Inside the RULES pragma, the usual layout rules
-- apply. The closing brace "#-}" has to be indented
-- at least one column more than the corresponding
-- open brace "{-#" of the pragma.
--
-- Variables used in rules must be declared using
-- "forall v1 v2 ... .". Types can be given in
-- the form "forall (v1::Integer) (v2::Num a => a) ... ."
-- A rule begins with the name of the rule (in quotation marks),
-- which is reported in the simplifier statistics output
-- in case the rule is applied.
--

{-# RULES
"addup/fromTo" forall x y. addup (fromTo x y) = if y >= x then (y*(y+1) - x*(x-1))`div`2 else 0
 #-}

-- Remove the above rule to make the computation "slow".

--
-- Rules can only be applied by the compiler when the respective
-- functions have not been inlined; therefore, we tell the compiler
-- not to inline the functions involved in our optimization rule.
--
{-# NOINLINE fromTo #-}
{-# NOINLINE addup #-}


main :: IO ()
main = print $ addup (fromTo 1 100000000)
