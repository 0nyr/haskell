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
lengthInvBin (L Z) = 1
lengthInvBin (O Z) = 1
lengthInvBin (O next) = 1 + lengthInvBin next
lengthInvBin (L next) = 1 + lengthInvBin next

tailInvBin :: InvBin -> InvBin
tailInvBin Z = Z
tailInvBin (O x) = x
tailInvBin (L x) = x

headInvBin :: InvBin -> InvBin
headInvBin Z = Z
headInvBin (O x) = (O Z)
headInvBin (L x) = (L Z)

-- left is appened to right
appendInvBin :: InvBin -> InvBin -> InvBin
appendInvBin Z y = y
appendInvBin x Z = x
appendInvBin (O x) y = O (appendInvBin x y)
appendInvBin (L x) y = L (appendInvBin x y)


adder :: InvBin -> InvBin -> InvBin
adder Z Z = Z 
adder Z (O Z) = (O Z)
adder (O Z) Z = (O Z)
adder Z (L Z) = (L Z)
adder (L Z) Z = (L Z)
adder (O Z) (O Z) = (O Z)
adder (O Z) (L Z) = (L Z) 
adder (L Z) (O Z) = (L Z) 
adder (L Z) (L Z) = (O Z)


fullAdder :: InvBin -> InvBin -> InvBin -> (InvBin, InvBin)
fullAdder a b c = 
    (aplusb, carry)
    where
        aplusb = adder a b
        carry = adder aplusb c


add :: InvBin -> InvBin -> InvBin
add x Z = x
add Z y = y
add x y =
    let 
        -- in: 2 InvBin to add, out: result (appened up to now) and carry 
        addRec :: InvBin -> InvBin -> (InvBin, InvBin) 
        addRec Z Z = (Z, Z)
        addRec a b = 
            let 
                (precedingSum, precedingCarry) = addRec (tailInvBin a) (tailInvBin b)
                (currentSum, currentCarry) = fullAdder (headInvBin a) (headInvBin b) precedingCarry
            in
                (appendInvBin currentSum precedingSum, currentCarry)

        -- we need to get the final carry so as to add it
        (res, finalCarry) = addRec x1goodLen y2goodLen
    in
        appendInvBin finalCarry res
    where
        -- we need the two InvBin to have the same length
        -- so, add heading O to the InvBin that needs it so that
        -- the two InvBin have same length
        lengthx = lengthInvBin x
        lengthy = lengthInvBin y

        maxLen = max lengthx lengthy
        minLen = min lengthx lengthy
        diffLen = maxLen - minLen

        extendWithZeros :: InvBin -> Int -> InvBin
        extendWithZeros invbin 0 = invbin
        extendWithZeros invbin remainingZerosToAdd =
            extendWithZeros (O invbin) (remainingZerosToAdd - 1)

        x1goodLen = if lengthx < maxLen then extendWithZeros x diffLen else x
        y2goodLen = if lengthy < maxLen then extendWithZeros y diffLen else y




instance Eq InvBin where
    Z == Z = True
    (O a) == (O b) = a == b
    (L a) == (L b) = a == b
    _ == _ = False

instance Ord InvBin where
    compare Z Z = EQ
    compare Z _ = LT
    compare _ Z = GT
    compare (O a) (O b) = compare a b
    compare (O _) _ = LT
    compare _ (O _) = GT
    compare (L a) (L b) = compare a b

instance Num InvBin where
    (+) = add
    -- No natural way to define multiplication and absolute value
    (*) = error "Multiplication not defined for InvBin"
    abs = error "Absolute value not defined for InvBin"
    signum = error "Signum not defined for InvBin"
    -- Defining fromInteger using the toInvBin function you've already defined
    fromInteger = toInvBin
    negate = error "Negation not defined for InvBin"




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

testAdd :: IO ()
testAdd = do
    -- Test 0 + 0 = 0
    myAssert $ add (toInvBin 0) (toInvBin 0) == toInvBin 0
    -- Test 0 + 1 = 1
    myAssert $ add (toInvBin 0) (toInvBin 1) == toInvBin 1
    -- Test 1 + 0 = 1
    myAssert $ add (toInvBin 1) (toInvBin 0) == toInvBin 1
    -- Test 1 + 1 = 2 (carry 1)
    myAssert $ add (toInvBin 1) (toInvBin 1) == toInvBin 2
    -- Test 2 + 1 = 3
    myAssert $ add (toInvBin 2) (toInvBin 1) == toInvBin 3
    -- Test 5 + 2 = 7
    myAssert $ add (toInvBin 5) (toInvBin 2) == toInvBin 7

main :: IO ()
main = do
    putStrLn "Week 6 exercices: [see below]"
    
    putStr "toInvBin 43: "
    print (toInvBin 43)

    -- Add the test for add function
    putStrLn "Testing add function: "
    testAdd

    print (toInvBin 1)
    print(add (toInvBin 1) (toInvBin 1))

