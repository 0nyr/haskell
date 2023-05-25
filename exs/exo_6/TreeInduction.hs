module TreeInduction where

data Tree = Leaf Int
          | Node Tree Int Tree
            deriving Show

sumT :: Tree -> Int
sumT (Leaf x)     = x                                   -- sumT.1
sumT (Node l x r) = sumT l + x + sumT r                 -- sumT.2

sumAcc :: Tree -> Int -> Int
sumAcc (Leaf x)     y = x+y                             -- sumAcc.1
sumAcc (Node l x r) y = sumAcc l (sumAcc r (x+y))       -- sumAcc.2


-- Proposition: sumT t + y = sumAcc t y

proof :: Tree -> Int -> [Int]
proof = undefined


testTree :: Tree
testTree =
    Node (Node (Leaf 1) 2 (Leaf 3))
         4
         (Node (Node (Leaf 5) 6 (Leaf 7))
               8
               (Leaf 9)
         )

