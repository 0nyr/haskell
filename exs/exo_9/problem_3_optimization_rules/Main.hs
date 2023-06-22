{-# OPTIONS_GHC -O -ddump-simpl-stats #-}
module Main where

import Control.Monad
import System.Environment

# ❮onyr ★ kenzael❯ ❮problem_3_optimization_rules❯❯ time ./Main 
# Running with n = 10
# 6290431
#
# real    0m0.017s
# user    0m0.001s
# sys     0m0.006s

-- $ is just function application
-- it makes the code more readable
-- negate $ negate 42 = negate (negate 42)

--
-- Data type "BTree a"
--
data BTree a = Leaf a                        -- leaf
             | Node (BTree a) a (BTree a)    -- inner node
               deriving (Eq, Show)

--
-- Produce a tree with 2^n leaves.
--
bigTree :: Integer -> BTree Integer
bigTree n = btHelp 1 n
    where
      btHelp val 0     = Leaf val
      btHelp val depth = Node (btHelp (2*val) (depth-1))
                              val
                              (btHelp (2*val+1) (depth-1))

--
-- mapBTree (as known)
--
mapBTree :: (a -> b) -> BTree a -> BTree b
mapBTree f (Leaf a)       = Leaf $ f a
mapBTree f (Node t1 a t2) = Node (mapBTree f t1) (f a) (mapBTree f t2)

--
-- foldBTree (as known)
--
foldBTree :: (a -> b) -> (b -> a -> b -> b) -> BTree a -> b
foldBTree leafF _nodeF (Leaf a)       = leafF a
foldBTree leafF  nodeF (Node t1 a t2) =
    nodeF (foldBTree leafF nodeF t1) a (foldBTree leafF nodeF t2)

--
-- A computation with several intermediate BTree structures.
--
bigComputation :: Integer -> Integer
bigComputation n =
    foldBTree id add3 $
    mapBTree (+1) $
    mapBTree (*3) $
    bigTree n
    where
      add3 a b c = a+b+c

--
-- Put optimization rules here which eliminate
-- the intermediate structures in 'bigComputation'.
--
-- For the second rule, remember that the map has to change the middle value
-- contained by the Tree.
{-# RULES
    "mapBTree/mapBTree" forall f g t.
    mapBTree f (mapBTree g t) = mapBTree (f . g) t
    "mapBTree/foldBTree" forall l n f t. 
    foldBTree l n (mapBTree f t) = foldBTree (l . f) (\x y z -> n x (f y) z) t
  #-}

--
-- Functions involved in optimization rules should not be inlined
-- (because after inlining, rules cannot fire anymore). Add
-- more functions here if the compiler gives you warnings about
-- rules that may not fire.
--
{-# NOINLINE mapBTree #-}
{-# NOINLINE foldBTree #-}

--
-- Read 'n' from the command line;
-- use n=10 when no parameter is given (or
-- the parameter is not an integer).
--
main :: IO ()
main = do
    args <- getArgs
    let n = case map reads args of
                ((x,""):_):_ -> x
                _        -> 10
    putStrLn ("Running with n = " ++ show n)
    print $ bigComputation n
