module Main where
--
-- Haskell's type system can help to verify proofs in an automated fashion.
-- By collecting the steps of a proof chain in a list, the type system ensures
-- that all expressions have the same type and each free variable is used with
-- the same type in every expression.
--

--
-- Example (from the lecture slides):
-- Show that for arbitrary finite lists xs,ys :: [a] and an arbitrary function
-- f :: a -> b the following holds:
--
--    map f xs ++ map f ys  =  map f (xs ++ ys)
--
--
-- Use the following equalities in the proof:
--
--    []     ++ ys  =  ys                -- (++).1
--    (x:xs) ++ ys  =  x : (xs++ys)      -- (++).2
--
--    map f []     = []                  -- map.1
--    map f (x:xs) = f x : map f xs      -- map.2
--

--
-- Induction proof for the above theorem using structural induction:
--
proof :: (a -> b) -> [a] -> [a] -> [[b]]

--
-- Base case: xs = []
--  f and ys are free variables in the claimed proposition.
--  xs is (cf. the where-declaration) an empty list.
--  Between the proof steps we give comments explaining
--  the respective step.
--
proof f [] ys =
    [map f xs ++ map f ys
    , -- xs = []
    map f [] ++ map f ys
    , -- map.1
    [] ++ map f ys
    , -- (++).1
    map f ys
    , -- (++).1
    map f ([]++ys)
    , -- xs = []
    map f (xs++ys)
    ]
    where
      xs = []

--
-- Inductive case: xs = z : zs
--  f, z, zs and ys are free variables in the claimed proposition.
--  xs is (cf. the where-declaration) the list z:zs.
--
proof f (z:zs) ys =
    [map f xs ++ map f ys
    , -- xs = z:zs
    map f (z:zs) ++ map f ys
    , -- map.2
    (f z : map f zs) ++ map f ys
    , -- (++).2
    f z : (map f zs ++ map f ys)
    , -- induction hypothesis
    f z : (map f (zs++ys))
    , -- map.2
    map f (z:(zs++ys))
    , -- (++).2
    map f ((z:zs)++ys)
    , -- xs = z:zs
    map f (xs++ys)
    ]
    where
      xs = z : zs


--
-- Task 1 (a)
--
-- Prove that
--     foldr f e (xs++ys) = f (foldr f e xs) (foldr f e ys)
-- holds for an arbitrary associative function f :: a->a->a with neutral
-- element e :: a and arbitrary finite lists xs, ys :: [a].
--

-- Induction
proof1a :: (a->a->a) -> a -> [a] -> [a] -> [a]
-- base case
-- NOTE: f e a = f a e = a (e neutral element)
proof1a f e xs@[] ys = 
        [
                f (foldr f e xs) (foldr f e ys),
                -- xs = []
                f (foldr f e []) (foldr f e ys),
                -- foldr.1
                f e (foldr f e ys),
                -- (++).1
                f e (foldr f e ([] ++ ys)),
                -- e neutral element through f
                foldr f e ([] ++ ys),
                -- xs = []
                foldr f e (xs ++ ys)
        ]
-- ladder case
-- NB: For this case, we need a lemme on f:
-- lemme: f a (f b c) = f (f a b) c
--      where f is a binary function (binary operator)
-- proof:
-- with ☉ denoting a binary ASSOCIATIVE operator
--  f a (f b c) = f a (b ☉ c)
--              = a ☉ (b ☉ c)
--              = (a ☉ b) ☉ c
--              = (f a b) ☉ c
--              = (f a b) c
proof1a f e xs@(z:zs) ys = 
        [
                f (foldr f e xs) (foldr f e ys),
                -- xs = z:zs
                f (foldr f e (z:zs)) (foldr f e ys),
                -- foldr.2
                f (f z (foldr f e zs)) (foldr f e ys),
                -- lemme: f a (f b c) = f (f a b) c
                f z (f (foldr f e zs) (foldr f e ys)),
                -- induction: f (foldr f e xs) (foldr f e ys) = foldr f e (xs++ys)
                f z (foldr f e (zs ++ ys)),
                -- foldr.2
                foldr f e (z: (zs ++ ys)),
                -- (++).2
                foldr f e ((z:zs) ++ ys),
                -- xs = z:zs
                foldr f e (xs ++ ys)
        ]


--
-- Task 1 (b)
--
data Tree a = Leaf a
            | Fork (Tree a) (Tree a)
              deriving (Eq, Show)

height :: Tree a -> Integer
height (Leaf _)     = 0                                -- height.1
height (Fork t1 t2) = 1 + max (height t1) (height t2)  -- height.2

nrForks :: Tree a -> Integer
nrForks (Leaf _)     = 0                            -- nrForks.1
nrForks (Fork t1 t2) = 1 + nrForks t1 + nrForks t2  -- nrForks.2

--
-- Prove that
--    nrForks t <= 2^(height t) - 1
-- holds for all finite trees t :: Tree a.
--

-- Induction
proof1b :: Tree a -> [Bool]
-- base case
proof1b t@(Leaf x) =
        [
                nrForks t <= 2^(height t) - 1,
                -- t = Leaf x
                nrForks (Leaf x) <= 2^(height (Leaf x)) - 1,
                -- nrForks.1
                0 <= 2^(height (Leaf x)) - 1,
                -- height.1
                0 <= 2^0 - 1,
                -- math
                0 <= 0
        ]
-- ladder case
proof1b t@(Fork l r) =
        [
                nrForks t <= 2^(height t) - 1,
                -- t = Fork l r
                nrForks (Fork l r) <= 2^(height (Fork l r)) - 1,
                -- nrForks.2
                1 + (nrForks l) + (nrForks r) <= 2^(height (Fork l r)) - 1,
                -- inductions:
                nrForks l <= 2^(height l) - 1,
                nrForks r <= 2^(height r) - 1,
                (nrForks l) + (nrForks r) <= 2^(height l) - 1 + 2^(height r) - 1,
                -- applying induction:
                1 + (nrForks l) + (nrForks r) <= 2^(height l) + 2^(height r) - 1,
                -- math
                2^(height l) + 2^(height r) - 1 <= 2^(max (height l) (height r)) + 2^(max (height l) (height r)) - 1,
                -- math
                2^(height l) + 2^(height r) - 1 <= 2*2^(max (height l) (height r)) - 1,
                -- math 
                2^(height l) + 2^(height r) - 1 <= 2^(1 + max (height l) (height r)) - 1,
                -- height.2
                2^(height l) + 2^(height r) - 1 <= 2^(height (Fork l r)) - 1,
                -- t = Fork l r
                2^(height l) + 2^(height r) - 1 <= 2^(height t) - 1,
                -- math
                1 + (nrForks l) + (nrForks r) <= 2^(height t) - 1,
                -- conclusion
                nrForks t <= 2^(height t) - 1
        ]


-- ##### code testing #####
testTree :: Tree Int
testTree = 
    Fork (Fork (Leaf 1) (Leaf 3))
         (Fork (Fork (Leaf 5) (Leaf 7))
               (Fork (Fork (Leaf 5) (Leaf 7))
                     (Leaf 9)
                     
               )
         )

tests1a :: [[Integer]]
tests1a =
    [
        proof1a (+) 0 [] [5,6],
        proof1a (+) 0 [1,2,3,4] [5,6],
        proof1a (*) 1 [1,2,3,4] [5,6]
    ]

tests1b :: [[Bool]]
tests1b =
    [
        proof1b testTree,
        proof1b (Leaf 42)
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
    mapM_ (myAssert . allEqual) tests1a
    mapM_ (myAssert . allEqual) tests1b


