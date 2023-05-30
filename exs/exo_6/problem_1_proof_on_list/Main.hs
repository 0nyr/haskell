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
-- Base case: xs = []
--  f and ys are free variables in the claimed proposition.
--  xs is an empty list. (Note: xs@[] is a so-called "as-pattern"
--  where "[]" is the actual pattern but the variable "xs" is also
--  bound to the value []).
--  Between the proof steps we give comments explaining
--  the respective step.
--
proof :: (a -> b) -> [a] -> [a] -> [[b]]
proof f xs@[] ys =
    [
        map f xs ++ map f ys
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
    [
        map f xs ++ map f ys
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

--
-- Subtask (a)
--
-- Prove that
--  length (map f xs) = length xs
-- holds for arbitrary but finite lists xs :: [a] and functions f :: a->b.

-- Induction proof
proofa :: (a -> b) -> [a] -> [Int]
-- base case: -- xs = []
proofa f xs@[] =
    [
        length (map f xs),
        -- xs = []
        length (map f []),
        -- map.1
        length [],
        -- xs = []
        length xs
    ]
-- ladder care: -- xs = z:zs
proofa f xs@(z:zs) =
    [
        length (map f xs),
        -- xs = z:zs
        length (map f (z:zs)),
        -- map.2
        length (f z: (map f zs)),
        -- length.2
        1 + length (map f zs),
        -- induction case
        1 + length zs,
        -- length.2
        length (z:zs),
        -- xs = z:zs
        length xs
    ]


--
-- Subtask (b)
--
-- Prove that
--     length (xs ++ ys) = length xs + length ys
-- holds for arbitrary but finite lists xs, ys :: [a].
--
-- Induction proof
proofb :: [a] -> [a] -> [Int]
-- base case: -- xs = []
proofb xs@[] ys =
    [
        length (xs ++ ys),
        -- xs = []
        length ([] ++ ys),
        -- (++).1
        0 + length ys,
        -- length.1
        length [] + length ys,
        -- xs = []
        length xs + length ys
    ]
-- ladder care: -- xs = z:zs
proofb xs@(z:zs) ys =
    [
        length (xs ++ ys),
        -- xs = z:zs
        length ((z:zs) ++ ys),
        -- (++).2
        length (z: (zs ++ ys)),
        -- length.2
        1 + length (zs ++ ys),
        -- induction case
        1 + length zs + length ys,
        -- lenght.2
        length (z:zs) + length ys,
        -- xs = z:zs
        length xs + length ys
    ]


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

-- Induction
proofc :: [a] -> [Int]
-- base case
proofc xs@[] =
    [
        length (powerlist xs),
        -- xs = []
        length (powerlist []),
        -- powerlist.1
        length [[]],
        -- length.2
        1 + length [],
        -- length.1
        1 + 0,
        -- math
        1
    ]
-- ladder case
proofc xs@(z:zs) = 
    [
        length (powerlist xs),
        -- xs = z:zs
        length (powerlist (z:zs)),
        -- powerlist.2
        length (map (z:) (powerlist zs) ++ powerlist zs),
        -- lengthconc (proofb)
        length (map (z:) (powerlist zs)) + length (powerlist zs),
        -- lengthmap (proofa)
        length (powerlist zs) + length (powerlist zs),
        -- math
        2*(length (powerlist zs)),
        -- induction case
        2*(2^(length zs)),
        -- math
        2^(length zs + 1),
        -- length.2
        2^(length (z:zs)),
        -- xs = z:zs
        2^(length xs)
    ]




-- ##### code testing #####
-- To test the proof, call the function for the proof
-- with arguments matching both the base case and the
-- inductive case.
-- For example:
tests :: [[[Integer]]]
tests =
    [
        proof (*2) []          [5,9,2,5,4],
        proof (+1) (4:[1,4,5]) [6,3,2]
        -- In each returned list, all elements should be equal
        -- (because we prove a theorem with an equality).
    ]

testsa :: [[Int]]
testsa =
    -- proofa: length (map f xs) = length xs
    [
        proofa (*3) [], 
        proofa (+7) [5,9,2,5,4]
    ]

testsb :: [[Int]]
testsb =
    -- proofa: length (map f xs) = length xs
    [
        proofb (4:[1,4,5]) [6,3,2],
        proofb [] [5,9,2,5,4]
    ]

testsc :: [[Int]]
testsc =
    -- proofa: length (map f xs) = length xs
    [
        proofc [],
        proofc [1,3,5,8,5,9,2,5,4]
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
    mapM_ (myAssert . allEqual) testsa
    mapM_ (myAssert . allEqual) testsb
    mapM_ (myAssert . allEqual) testsc


