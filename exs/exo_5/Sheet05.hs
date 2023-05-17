module Sheet05 where

import DC          -- divide & conquer skeleton

import Data.List   -- for list combinators

--
-- Problem 1
--

fromTo = undefined

mymap = undefined

foldUnfoldr f x g y = undefined

testFUF :: [Bool]
testFUF = [fromTo 1 5 == [1..5::Int]
          ,mymap (^2) [1..5] == [1,4,9,16,25]
          ,foldr (+) 0 (unfoldr g 1) == foldUnfoldr (+) 0 g 1
          ]
    where
      g x = if x>5 then Nothing else Just (x,x+1)


--
-- Problem 2
--

data Tree = E                 -- empty leaf
          | N Tree Char Tree  -- inner node
            deriving Eq

exampleTree :: Tree
exampleTree = N (N E 'a' E) 'b' E

preorder :: Tree -> String
preorder = undefined

fromPreorder :: String -> Tree
fromPreorder = undefined

testPO :: [Bool]
testPO = [preorder exampleTree == "ba..."
         ,fromPreorder "ba..." == exampleTree
         ]


--
-- Problem 3
--

data BTree a = Leaf a                        -- leaf with a value
             | Node (BTree a) a (BTree a)    -- inner node with a value
               deriving Show

-- a search tree w.r.t. (<=)
tree1 :: BTree Int
tree1 =
    Node (Node (Leaf 2)
               3
               (Leaf 4)
         )
         5
         (Node (Node (Leaf 6)
                     7
                     (Leaf 8)
               )
               9
               (Leaf 10)
         )

-- not a search tree w.r.t. (<=)
tree2 :: BTree Int
tree2 =
    Node (Node (Leaf 2)
               3
               (Leaf 4)
         )
         5
         (Node (Node (Leaf 6)
                     7
                     (Leaf 11)
               )
               9
               (Leaf 10)
         )


mapBTree = undefined

foldBTree = undefined

postOrder :: BTree a -> [a]
postOrder = undefined

isSearchTree :: (a -> a -> Bool) -> BTree a -> Bool
isSearchTree = undefined

testST :: [Bool]
testST = [isSearchTree (<=) tree1
         ,not $ isSearchTree (<=) tree2
         ]


--
-- Problem 4
--

mergesort :: Ord a => [a] -> [a]
mergesort = undefined

testMS :: [Bool]
testMS = [mergesort xs == sort xs]
    where
      xs = [3, 9, 4, 7, -2, 8, 5, 1]
