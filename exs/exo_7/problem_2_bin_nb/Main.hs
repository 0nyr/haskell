module Main where

data InvBin = Z         -- 0
            | O InvBin  -- n -> 2*n
            | L InvBin  -- n -> 2*n+1
            deriving Show

fromInvBin :: InvBin -> Integer
fromInvBin Z     = 0                     -- fromInvBin.1
fromInvBin (O x) = 2 * fromInvBin x      -- fromInvBin.2
fromInvBin (L x) = 2 * fromInvBin x + 1  -- fromInvBin.3

inc :: InvBin -> InvBin
inc Z     = L Z         -- inc.1
inc (O x) = L x         -- inc.2
inc (L x) = O (inc x)   -- inc.3

-- a)
-- build from the bottom
-- 1. 43    Z
-- 2. 21    L(Z)
-- 3. 10    O(L(Z))
-- 4. 5     ...
toInvBin :: Integer -> InvBin
toInvBin 0 = Z
toInvBin x = 
    let 
        toInvBinRec :: Integer -> InvBin -> InvBin 
        toInvBinRec 0 t = t 
        toInvBinRec r t  
            | r `mod` 2 == 1 = toInvBinRec (r `div` 2) (L t)
            | otherwise = toInvBinRec (r `div` 2) (O t)
    in toInvBinRec x Z

--
-- Task 2(d)
--
-- Prove using structural induction that for all finite x :: InvBin
-- the following holds:
--     fromInvBin (inc x) == fromInvBin x + 1
-- (See the file InductionProof.hs in Stud.IP for an explanation how
-- to test your proof in an automated fashion.)
--


lengthInvBin :: InvBin -> Int
lengthInvBin Z = 0
lengthInvBin L Z = 1
lengthInvBin O Z = 1
lengthInvBin (O next) = 1 + lengthInvBin next
lengthInvBin (L next) = 1 + lengthInvBin next

add :: InvBin -> InvBin -> InvBin
add x Z = x
add Z y = y
add x y =
    let 
        adder :: InvBin -> InvBin -> InvBin
        adder Z Z = Z   -- 0
        adder Z a = a   -- 0
        adder a Z = a   -- 0
        adder a b = L Z -- 1

        fullAdder :: InvBin -> InvBin -> InvBin -> (InvBin, InvBin)
        fullAdder a b c = 
            (aplusb, carry)
            where
                aplusb = adder a b
                carry = adder aplusb c
        
        addRec :: InvBin -> InvBin -> InvBin -> InvBin -> InvBin
        addRec Z Z res r =
            case r of
                O _ -> res
                L _ -> L res
        addRec left right res carry = 
            let 
                (currentLeft, nextLeft) = case left of
                    O next -> (O Z, next)
                    L next -> (L Z, next)
                (currentRight, nextRight) = case right of
                    O next -> (O Z, next)
                    L next -> (L Z, next)
                
                (fullAddRes, fullAddCarry) = fullAdder currentLeft currentRight carry
            in
                addRec ( res)

        
    in

    where
        -- add heading O to the InvBin that needs it so that
        -- the two InvBin have same length
        maxLen = max (lengthInvBin x) (lengthInvBin y)
        minLen = min (lengthInvBin x) (lengthInvBin y)
        diffLen = maxLen - minLen

        x1goodLen = extendWithZeros x maxLen
        y2goodLen = extendWithZeros y maxLen

        extendWithZeros invbin remainingZerosToAdd acc
            ...




-- ##### code testing #####
proof2d :: InvBin -> [Integer]
proof2d = undefined


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
    
    putStr "toInvBin 43: "
    print (toInvBin 43)



