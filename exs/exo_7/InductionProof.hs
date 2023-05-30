module InductionProof where
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


tests :: [[[Integer]]]
tests =
--
-- To test the proof, call the function for the proof
-- with arguments matching both the base case and the
-- inductive case.
-- For example:
        (proof (*2) []          [5,9,2,5,4])
        :
--  and
        (proof (+1) (4:[1,4,5]) [6,3,2])
        :
-- In each returned list, all elements should be equal
-- (because we prove a theorem with an equality).
--
        []


--
-- Task 1 (a)
--
-- Prove that
--     foldr f e (xs++ys) = f (foldr f e xs) (foldr f e ys)
-- holds for an arbitrary associative function f :: a->a->a with neutral
-- element e :: a and arbitrary finite lists xs, ys :: [a].
--

proof1a :: (a->a->a) -> a -> [a] -> [a] -> [a]
proof1a = undefined

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

proof1b :: Tree a -> [Integer]
proof1b = undefined
