module Main where

data Tree = Leaf Int
          | Node Tree Int Tree
            deriving Show

sumT :: Tree -> Int
sumT (Leaf x)     = x                                   -- sumT.1
sumT (Node l x r) = sumT l + x + sumT r                 -- sumT.2

sumAcc :: Tree -> Int -> Int
sumAcc (Leaf x)     y = x+y                             -- sumAcc.1
sumAcc (Node l x r) y = sumAcc l (sumAcc r (x+y))       -- sumAcc.2


-- Proposition: 
-- forall t. (forall y. sumT t + y = sumAcc t y)

-- Induction
proof :: Tree -> Int -> [Int]
-- base case
proof t@(Leaf x) y = 
      [
            (sumT t) + y,
            -- t = Leaf x
            (sumT (Leaf x)) + y,
            -- sumT.1
            x + y,
            -- sumAcc.1
            sumAcc (Leaf x) y,
            -- t = Leaf x
            sumAcc t y
      ]
-- ladder case
-- Note: 2 recursive Tree in Tree def means 2 induction cases needed
-- t = Node l x r, for any x::Int, l,r::Tree
-- Induction cases:
--    1. l = Leaf x, r = Leaf y
--       forall y. sumT l + a = sumAcc l a
--    2. l = Node l1 x1 r1, r = Leaf y
--       forall y. sumT l + a = sumAcc l a
-- NOTE: So here, we can for any y, since we are doing induction on t
proof t@(Node l x r) y =
      [
            (sumT t) + y,
            -- t = Node l x r
            (sumT (Node l x r)) + y,
            -- sumT.2
            (sumT l) + x + (sumT r) + y,
            -- re arrange (associativity and commutativity of addition)
            (sumT l) + ((sumT r) + (x + y)),
            -- induction case: sumT l + a = sumAcc l a
            sumAcc l ((sumT r) + (x + y)),
            -- induction case: sumT r + b = sumAcc r b
            sumAcc l (sumAcc r (x + y)),
            -- sumAcc.2
            sumAcc (Node l x r) y,
            -- t = Node l x r
            sumAcc t y
      ]



-- ##### code testing #####
testTree :: Tree
testTree =
    Node (Node (Leaf 1) 2 (Leaf 3))
         4
         (Node (Node (Leaf 5) 6 (Leaf 7))
               8
               (Leaf 9)
         )


tests =
    [
        proof testTree 3,
        proof (Leaf 5) 2
    ]


myAssert :: Bool -> IO ()
myAssert True = putStrLn "🟢 assertion ok"
myAssert False = putStrLn "🔴 assertion failed"

-- Check if all elements in a list are equal
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

main :: IO ()
main = do
    putStrLn "Week 6 exercices: [see below]"
    mapM_ (myAssert . allEqual) tests



