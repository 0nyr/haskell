module BigEndian where
-- WARN: In this file, we consider InvBin in Little Endian...
-- (...which makes things much much harder...)


data InvBin = Z         -- 0
            | O InvBin  -- n -> 2*n
            | L InvBin  -- n -> 2*n+1
            deriving Show

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

-- WARNING: In the following code, we have understood that 
-- InvBin was in BigEndian, but the tutorial was supposed to 
-- be little endian... which is much simpler to do...
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


carry :: InvBin -> InvBin -> InvBin -> InvBin
carry (L Z) (L Z) _= L Z
carry (L Z) _ (L Z)= L Z
carry _ (L Z) (L Z)= L Z
carry _ _ _ = O Z


fullAdder :: InvBin -> InvBin -> InvBin -> (InvBin, InvBin)
fullAdder a b c = 
    (aplusb, carryRes)
    where
        aplusb = adder a (adder b c)
        carryRes = carry a b c


extendWithZeros :: InvBin -> Int -> InvBin
extendWithZeros invbin 0 = invbin
extendWithZeros invbin remainingZerosToAdd =
    extendWithZeros (O invbin) (remainingZerosToAdd - 1)

add :: InvBin -> InvBin -> InvBin
add x Z = x
add Z y = y
add x y =
    let 
        -- in: 2 InvBin to add, out: result (appened up to now) and carry 
        -- L Z + L Z
        -- 
        addRec :: InvBin -> InvBin -> (InvBin, InvBin) 
        addRec Z Z = (Z, Z)
        addRec a b = 
            let 
                (precedingSum, precedingCarry) = addRec (tailInvBin a) (tailInvBin b)
                (currentSum, currentCarry) = fullAdder (headInvBin a) (headInvBin b) precedingCarry
            in
                (appendInvBin currentSum precedingSum, currentCarry)

        -- we need the two InvBin to have the same length
        -- so, add heading O to the InvBin that needs it so that
        -- the two InvBin have same length
        lengthx = lengthInvBin x
        lengthy = lengthInvBin y

        maxLen = max lengthx lengthy
        minLen = min lengthx lengthy
        diffLen = maxLen - minLen

        x1goodLen = if lengthx < maxLen then extendWithZeros x diffLen else x
        y2goodLen = if lengthy < maxLen then extendWithZeros y diffLen else y

        -- we need to get the final carry so as to add it
        (res, finalCarry) = addRec x1goodLen y2goodLen
    in
        case finalCarry of 
            Z -> res
            O _ -> res
            L _ -> appendInvBin finalCarry res

extendRightWithNZeros :: InvBin -> Int -> InvBin
extendRightWithNZeros x 0 = x
extendRightWithNZeros x n = 
    extendRightWithNZeros newX (n - 1)
    where 
        newX = (appendInvBin x (O Z))



multiply :: InvBin -> InvBin -> InvBin
multiply Z _ = Z
multiply _ Z = Z
multiply x y = 
    let 
        multiplyWithBit :: InvBin -> InvBin -> InvBin
        multiplyWithBit Z x = (O Z)
        multiplyWithBit (O Z) x = (O Z)
        multiplyWithBit (L Z) x = x

        currentCalc = multiplyWithBit (headInvBin x) y
        currentCalcWithZeros = extendRightWithNZeros currentCalc ((lengthInvBin x) - 1)
    in 
        add currentCalcWithZeros (multiply (tailInvBin x) y)


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
    (*) = multiply
    abs = error "Absolute value not defined for InvBin"
    signum = error "Signum not defined for InvBin"
    -- Defining fromInteger using the toInvBin function you've already defined
    fromInteger = toInvBin
    negate = error "Negation not defined for InvBin"


-- ##### code testing #####
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
    -- Test 240 + 44 = 284
    myAssert $ add (toInvBin 240) (toInvBin 44) == toInvBin 284

testMultiply :: IO ()
testMultiply = do
    -- Test 0 * 0 = 0
    myAssert $ multiply (toInvBin 0) (toInvBin 0) == toInvBin 0
    -- Test 0 * 1 = 0
    myAssert $ multiply (toInvBin 0) (toInvBin 1) == toInvBin 0
    -- Test 1 * 0 = 0
    myAssert $ multiply (toInvBin 1) (toInvBin 0) == toInvBin 0
    -- Test 1 * 1 = 1
    myAssert $ multiply (toInvBin 1) (toInvBin 1) == toInvBin 1
    -- Test 2 * 2 = 4
    myAssert $ multiply (toInvBin 2) (toInvBin 2) == toInvBin 4
    -- Test 3 * 3 = 9
    myAssert $ multiply (toInvBin 3) (toInvBin 3) == toInvBin 9
    -- Test 5 * 5 = 25
    myAssert $ multiply (toInvBin 5) (toInvBin 5) == toInvBin 25
    -- Test 10 * 10 = 100
    myAssert $ multiply (toInvBin 10) (toInvBin 10) == toInvBin 100


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


    print (toInvBin 1)
    print(add (toInvBin 1) (toInvBin 1))

    putStrLn "Testing extendRightWithNZeros function: "
    print (extendRightWithNZeros (toInvBin 2) 4)

    -- Add the test for multiply function
    putStrLn "Testing multiply function: "
    testMultiply

    print (multiply (toInvBin 4) (toInvBin 4))
