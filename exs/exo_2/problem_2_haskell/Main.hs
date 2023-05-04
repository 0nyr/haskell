module Main where

import Data.Ratio
import GHC.Driver.Session (xopt, DynFlags (callerCcFilters))

optimalX :: Double -> Double -> Double -> Maybe Double
optimalX a b c 
    | not aNull = Just (-b/(2*a))
    | aNull = Nothing
    where aNull = a == 0

-- correction
optimalXCorrection :: Rational -> Rational -> Rational -> Maybe Rational
optimalXCorrection a b c 
    | a /= 0 = Just (-b/(2*a))
    | otherwise = Nothing -- this is equivalent to write 'True', otherwise is a global var with value True

-- correction
optimalXCorrection2 :: (Eq t, Fractional t) => t -> t -> t -> t
optimalXCorrection2 0 b c = error "optimalX: a must not be 0" -- just for the example on how to use 'error'
optimalXCorrection2 a b c = -b/(2*a)


f :: Double -> Double -> Double -> Double -> Double
f a b c x = a*(x^2) + b*x + c

optimalF :: Double -> Double -> Double -> Maybe Double
optimalF a b c = let maybeOptimal = optimalX a b c 
    in case maybeOptimal of
        Just optimal -> Just (f a b c optimal)
        Nothing -> Nothing

-- correction
optimalFCorrection :: (Eq t, Fractional t) => t -> t -> t -> t
optimalFCorrection 0 b c = error "OptimalFCorrection: a must not be 0"
optimalFCorrection a b c = f xopt
    where
        xopt = optimalXCorrection2 a b c 
        f x = a*x^2 + b*x + c

optimal :: Double -> Double -> Double -> (Maybe Double, Maybe Double)
optimal a b c =
    (optimalX a b c, optimalF a b c)

-- correction
optimalCorrection :: (Eq t, Fractional t) => t -> t -> t -> (t, t)
optimalCorrection 0 _ _ = error "optimalCorrection: a must not be O"
-- the compiler is smart to evaluate once and reuse the result enough since optimalX has no side effect (this is Haskell, not Java)
-- this is considered good optimized code in Haskell
optimalCorrection a b c = (optimalXCorrection2 a b c, optimalFCorrection a b c)


optimalFromOptimalX :: Double -> Double -> Double -> Maybe (Double, Double)
optimalFromOptimalX a b c = case maybe_optimal of
    Just optimal -> Just (optimal, f a b c optimal)
    Nothing -> Nothing
    where maybe_optimal = optimalX a b c
                    

main :: IO ()
main = do
  putStr "optimalX 5 4 (-5): " 
  print (optimalX 5 4 (-5))

  putStr "optimalX 0 4 (-5): "
  print (optimalX 0 4 (-5))

  putStr "optimalF 5 4 (-5): " 
  print (optimalF 5 4 (-5))

  putStr "optimalF 0 4 (-5): "
  print (optimalF 0 4 (-5))

  putStr "optimal 5 4 (-5): " 
  print (optimal 5 4 (-5))

  putStr "optimal 0 4 (-5): "
  print (optimal 0 4 (-5))

  putStr "optimalFromOptimalX 5 4 (-5): " 
  print (optimalFromOptimalX 5 4 (-5))

  putStr "optimalFromOptimalX 0 4 (-5): "
  print (optimalFromOptimalX 0 4 (-5))

  putStr "optimalXCorrection 3 4 (-2): "
  print (optimalXCorrection 3 4 (-2))

  putStr "optimalXCorrection 5 4 (-5): " 
  print (optimalXCorrection 5 4 (-5))

  putStr "optimalXCorrection2 3 4 (-2): "
  print (optimalXCorrection2 3 4 (-2))

  putStr "optimalXCorrection2 5 4 (-5): " 
  print (optimalXCorrection2 5 4 (-5))
