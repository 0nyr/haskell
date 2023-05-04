{-# LANGUAGE ScopedTypeVariables #-} -- enable forall
{-# LANGUAGE FlexibleInstances #-} -- enable the definition of instance from base type like bool (or [Bool])
{-# LANGUAGE InstanceSigs #-} -- enable the type annotation on the instance (lisibility)
module Main where
sort2 :: Ord a => a -> a -> (a, a)
sort2 a b
    | aHigher = (b, a)
    | not aHigher = (a, b)
    where aHigher = a > b

sort3 :: Ord a => a -> a -> a -> (a, a, a)
sort3 a b c =
    let firstA = sort2 a b
    in case firstA of
        (la, ha) ->
            let firstC = sort2 c la
            in case firstC of
                (lc,hc) ->
                    if lc == c then (c, la, ha)
                    else let secondA = sort2 c ha
                    in case secondA of
                        (lc2, hc2) ->
                            if lc2 == c then (la, c, ha)
                            else (la, ha, c)

nTimes:: Int -> (a -> a) -> (a -> a)
nTimes n f =
    if n <= 1 then f
    else nTimes (n - 1) f . f

mul2 :: Num a => a -> a
mul2 x = x * 2
mul32 :: Integer -> Integer
mul32 = nTimes 5 mul2

data BoolFun = BF { numberArgs::Int, bfun::[Bool]->Bool }

toBoolList :: Int -> Int -> [Bool]
toBoolList n size = -- compute binary
    let new_size = size-1
        reste = n `mod` 2
        resteBool = reste == 1 -- transform the bit into bool
        division = n `div` 2
    in if n <= 1 then resteBool : replicate new_size False -- fill with false (0)
    else resteBool : toBoolList division new_size

generateBoolCombi :: Int -> [[Bool]]
generateBoolCombi n =
    let
        toBoolN :: Int -> [Bool]
        toBoolN x = toBoolList x n -- transform the fct to be mapped arround the list
    in map toBoolN [0..(2^n - 1)]

isSatisfiableRec :: BoolFun -> [[Bool]] -> Maybe [Bool]
isSatisfiableRec boolF toTest =
    case toTest of
        [] -> if bfun boolF [] then Just []
              else Nothing
        [a] -> -- base case
            let isSatis = (bfun boolF) a
            in if isSatis then Just a
            else Nothing
        (a:tail) ->
            let isSatis = (bfun boolF) a
            in if isSatis then Just a
            else isSatisfiableRec boolF tail -- recurve on the toTest list

isSatisfiable :: BoolFun -> Maybe [Bool]
isSatisfiable boolF =
    -- call the recurve one
    isSatisfiableRec boolF (generateBoolCombi (numberArgs boolF))

------------------------------satisfiable v2--------------
-- enum list of a
instance forall a .(Bounded a, Enum a) => Enum [a] where -- must use forall to use a in scope
    fromEnum :: (Bounded a, Enum a) => [a] -> Int
    fromEnum [] = -1
    fromEnum [x] = fromEnum x --from bool to int
    fromEnum (x:tail) =
       let scale = fromEnum (maxBound::a) - fromEnum (minBound::a) + 1
       in fromEnum x * (scale ^ length tail) + fromEnum tail

    toEnum :: (Bounded a, Enum a) => Int -> [a]
    toEnum i =
        if i == -1 then []
        else
            let scale = fromEnum (maxBound::a) - fromEnum (minBound::a) + 1 -- bool ex : 1 to 0 = 1, miss 1
                aToEnumFn::Int -> a -- precise the type to not confuse the compilator beetween int to a and int to [a]
                aToEnumFn x = toEnum x
                listAToEnum::Int -> [a]
                listAToEnum x = toEnum x
                resteEnum = aToEnumFn (i `mod` scale)
            in if i < scale then [resteEnum]
            else  resteEnum : listAToEnum (i `div` scale)

-- enumerate all the boolean list (binary map) (keeping this to have an example of instance on primary type)
{- instance Enum [Bool] where
    fromEnum :: [Bool] -> Int
    fromEnum [] = -1
    fromEnum [x] = fromEnum x
    fromEnum (x:tail) = fromEnum x * (2 ^ length tail) + fromEnum tail

    toEnum :: Int -> [Bool]
    toEnum i =
        let reste = i `mod` 2
            resteBool = reste == 1 -- transform the bit into bool
            division = i `div` 2
            listAToEnum::Int -> [Bool]
            listAToEnum x = toEnum x
        in if i <= 1 then [resteBool]
        else resteBool : listAToEnum division -}

-- normalize the list
normalizeBin :: [Bool] -> Int -> [Bool]
normalizeBin list n =
    let size_tail = (n - length list)
    in list ++ replicate size_tail False
normalize :: [[Bool]] -> Int -> [[Bool]]
normalize list n =
    let
        normalizeFn ::[Bool] -> [Bool]
        normalizeFn list = normalizeBin list n
    in map normalizeFn list

isSatisfiable2 :: BoolFun -> Maybe [Bool]
isSatisfiable2 BF {numberArgs=n, bfun=fct} = -- match arg
    let ok = [x | x <- normalize [[False]..(replicate n True)] n, fct x]
        isOk = not (null ok) -- list not empty
    in if isOk then Just (head ok)
    else if fct [] then Just []
        else Nothing

------------------------------satisfiable v3 --------------
generateBoolCombi2 :: Int -> [[Bool]]
generateBoolCombi2 0 = [[]]
generateBoolCombi2 n = 
    let prec = generateBoolCombi2 (n-1)
        mapTrue list =  True : list 
        mapFalse list =  False : list
    in map mapTrue prec ++ map mapFalse prec

main = do
    putStr "sort2 3 4 : "
    print (sort2 3 4)

    putStr "sort2 4 3 : "
    print (sort2 4 3)

    putStr "sort3 4 3 2 : "
    print (sort3 4 3 2)

    putStr "sort3 4 2 3 : "
    print (sort3 4 2 3)

    putStr "sort3 3 2 4 : "
    print (sort3 3 2 4)

    putStr "sort3 3 4 2 : "
    print (sort3 3 4 2)

    putStr "sort3 3 4 3 : "
    print (sort3 3 4 3)

    putStr "mul32 3 : "
    print (mul32 3)

    let combi = generateBoolCombi 5

    print combi


    let g [x,y] = x && not y

    print (isSatisfiable (BF { numberArgs=2, bfun=g }))

    let h [x,y,z] = x && not x

    print (isSatisfiable (BF { numberArgs=3, bfun=h }))

    putStr "test list enum : "
    print [[False]..[True, True, True]]
    putStr "2nd test list enum : "
    print [[False]..[True, True, True, True, True]]

    putStr "2nd test list enum and normalize : "
    print (normalize [[False]..[True, True, True]] 3)

    putStr "test 2nd isSatisfiable : "
    print (isSatisfiable2 (BF { numberArgs=2, bfun=g }))
    print (isSatisfiable2 (BF { numberArgs=3, bfun=h }))

    putStr "test the recursive building of combi : "
    print (generateBoolCombi2 4)