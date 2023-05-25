module ListInduction where
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
--  xs is an empty list. (Note: xs@[] is a so-called "as-pattern"
--  where "[]" is the actual pattern but the variable "xs" is also
--  bound to the value []).
--  Between the proof steps we give comments explaining
--  the respective step.
--
proof f xs@[] ys =
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

--
-- Inductive case: xs = z : zs
--  f, z, zs and ys are free variables in the claimed proposition.
--  xs is the list z:zs. (Again, we use an "as-pattern": "z:zs"
--  is the actual pattern but the variable "xs" also refers to "z:zs".)
--
proof f xs@(z:zs) ys =
    -- Induction Hypothesis: map f zs ++ map f ys == map f (zs ++ ys)
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
-- Subtask (a)
--
-- Prove that
--  length (map f xs) = length xs
-- holds for arbitrary but finite lists xs :: [a] and functions f :: a->b.
--

proofa :: (a -> b) -> [a] -> [Int]
proofa = undefined


--
-- Subtask (b)
--
-- Prove that
--     length (xs ++ ys) = length xs + length ys
-- holds for arbitrary but finite lists xs, ys :: [a].
--

proofb :: [a] -> [a] -> [Int]
proofb = undefined


--
-- Subtask (c)
--
-- Prove that
--     length (powerlist xs) = 2^(length xs)
-- holds for arbitrary but finite lists xs :: [a].
--

powerlist :: [a] -> [[a]]
powerlist []       = [[]]                                       -- powerlist.1
powerlist (x:xs) = map (x :) (powerlist xs) ++ powerlist xs     -- powerlist.2

proofc :: [a] -> [Int]
proofc = undefined

