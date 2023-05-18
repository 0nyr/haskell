module Main where

import Debug.Trace

data BTree a = Leaf a
             | Node (BTree a) a (BTree a)
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

-- not a search tree w.r.t. (<=) 
tree3 :: BTree Int
tree3 =
    Node (Node (Leaf 2)
               3
               (Leaf 4)
         )
         5
         (Node (Node (Leaf 6)
                     7
                     (Leaf 5)
               )
               9
               (Leaf 10)
         )

-- a)
mapBTree :: (a -> a) -> BTree a -> BTree a
mapBTree f (Leaf x) = Leaf (f x)
mapBTree f (Node treeL x treeR) =
    Node (mapBTree f treeL) (f x) (mapBTree f treeR)

-- b)
foldBTree :: (acc -> a -> acc) -> (acc -> a -> acc) -> acc -> BTree a -> acc
foldBTree fNode fLeaf accum (Leaf x) = fLeaf accum x
foldBTree fNode fLeaf accum (Node treeL x treeR) =
    foldBTree fNode fLeaf accumL treeR
    where
        -- start left side
        accumL = foldBTree fNode fLeaf (fNode accum x) treeL

-- c)
-- postOrder = let post x xss = concat xss ++ [x]
--      in foldTree post
postOrder :: BTree a -> [a]
postOrder (Leaf x) = [x]
postOrder tree =
    foldBTree post post [] tree
    where
        post accum x = accum ++ [x]

-- d)
-- not optimized
isSearchTreeShow :: (Bounded a, Ord a, Show a) => (a -> a -> Bool) -> BTree a -> Bool
isSearchTreeShow fOrder (Leaf x) = True
isSearchTreeShow fOrder (Node treeL x treeR)  =
    trace ("maxL: " ++ show maxL) $      -- debug
    trace ("minR: " ++ show minR) $    -- debug
        ((maxL `fOrder` x) && (x `fOrder` minR)) &&
        (isSearchTreeShow fOrder treeL && isSearchTreeShow fOrder treeR)
    where
        maxL = foldBTree max max minBound treeL
        minR = foldBTree min min maxBound treeR

isSearchTree :: (Bounded a, Ord a) => (a -> a -> Bool) -> BTree a -> Bool
isSearchTree fOrder (Leaf x) = True
isSearchTree fOrder (Node treeL x treeR)  =
    ((maxL `fOrder` x) && (x `fOrder` minR)) &&
    (isSearchTree fOrder treeL && isSearchTree fOrder treeR)
    where
        maxL = foldBTree max max minBound treeL
        minR = foldBTree min min maxBound treeR


myAssert :: Bool -> IO ()
myAssert True = putStrLn "🟢 assertion ok"
myAssert False = putStrLn "🔴 assertion failed"

main :: IO ()
main = do
    putStrLn "Week 5 exercices: [see below]"

    -- a)
    putStrLn "mapBTree f tree1: [see below]"
    print (mapBTree (^2) tree1)

    -- b)
    print tree1
    print (foldBTree (+) (+) 0 tree1)
    print (foldBTree (+) (-) 0 tree1)
    print (foldBTree (+) (*) 0 tree1)
    myAssert (foldBTree (+) (+) 1 tree1 == 1+2+3+4+5+6+7+8+9+10)
    myAssert (foldBTree (+) (-) 1 tree1 == 1 + ((-2) +3+ (-4)) +5+ (((-6) +7+ (-8)) +9+ (-10)))
    myAssert (foldBTree (+) (*) 1 tree1 == ((1+5+3)*2*4 + 9+7)*6*8*10)

    -- c)
    print tree1
    print (postOrder tree1)

    -- d)
    print (isSearchTree (<=) tree1)
    print (isSearchTree (<=) tree2)
    print (isSearchTree (<=) tree3)

    myAssert (isSearchTree (<=) tree1)
    myAssert (not $ isSearchTree (<=) tree2)
    myAssert (not $ isSearchTree (<=) tree3)
