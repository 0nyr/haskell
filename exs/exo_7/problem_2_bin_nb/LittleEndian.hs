module LittleEndian where

data InvBin = Z         -- 0
            | O InvBin  -- n -> 2*n
            | L InvBin  -- n -> 2*n+1
            deriving Show

fromInvBin :: InvBin -> Integer
fromInvBin Z     = 0                     -- fromInvBin.1
fromInvBin (O x) = 2 * fromInvBin x      -- fromInvBin.2
fromInvBin (L x) = 2 * fromInvBin x + 1  -- fromInvBin.3

toInvBin :: Int -> InvBin
toInvBin 0 = Z
toInvBin x
    | x `mod` 2 == 0 = O (toInvBin (x `div` 2))
    | otherwise      = L (toInvBin ((x - 1) `div` 2))


inc :: InvBin -> InvBin
inc Z     = L Z         -- inc.1
inc (O x) = L x         -- inc.2
inc (L x) = O (inc x)   -- inc.3

-- WARNING: In this proof, we are using InvBin as described and 
-- intended, in LittleEndian format. 
-- So the proof is correct from what is expected by the cla
-- Induction
proof2d :: InvBin -> [Integer]
-- base case
proof2d x@Z =
    [
        fromInvBin (inc x),
        -- x = Z
        fromInvBin (inc Z),
        -- inc.1
        fromInvBin (L Z),
        -- fromInvBin.3
        2*(fromInvBin Z) + 1,
        -- fromInvBin.1
        2*0 + 1,
        -- math
        0 + 1,
        -- fromInvBin.1
        fromInvBin Z + 1,
        -- x = Z
        fromInvBin x + 1
    ]
-- pseudo base case (ladder case on pseudo 0 case)
proof2d x@(O y) = 
    [
        fromInvBin (inc x),
        -- x = (O y)
        fromInvBin (inc (O y)),
        -- inc.2
        fromInvBin (L y),
        -- fromInvBin.3
        2*(fromInvBin y) + 1,
        -- fromInvBin.2
        fromInvBin (O y) + 1,
        -- x = (O y)
        fromInvBin x + 1
    ]
-- ladder case
proof2d x@(L y) = 
    [
        fromInvBin (inc x),
        -- x = (L y)
        fromInvBin (inc (L y)),
        -- inc.3
        fromInvBin (O (inc y)),
        -- fromInvBin.2
        2*(fromInvBin (inc y)),
        -- induction: fromInvBin (inc a) == fromInvBin a + 1
        2*(fromInvBin y + 1),
        -- math
        2*(fromInvBin y) + 2,
        -- math 
        2*(fromInvBin y) + 1 + 1,
        -- fromInvBin.3
        fromInvBin (L y) + 1,
        -- x = (L y)
        fromInvBin x + 1
    ]

tests =
    [
        proof2d Z,
        proof2d (L Z),
        proof2d (O Z),
        proof2d (toInvBin 0),
        proof2d (toInvBin 1),
        proof2d (toInvBin 2),
        proof2d (toInvBin 3),
        proof2d (toInvBin 42)
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