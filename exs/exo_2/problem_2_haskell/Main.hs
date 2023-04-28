module Main where

optimalX :: Double -> Double -> Double -> Maybe Double
optimalX a b c 
    | not aNull = Just (-b/(2*a))
    | aNull = Nothing
    where aNull = a == 0

f :: Double -> Double -> Double -> Double -> Double
f a b c x = a*(x^2) + b*x + c

optimalF :: Double -> Double -> Double -> Maybe Double
optimalF a b c = let maybeOptimal = optimalX a b c 
    in case maybeOptimal of
        Just optimal -> Just (f a b c optimal)
        Nothing -> Nothing

optimal :: Double -> Double -> Double -> (Maybe Double, Maybe Double)
optimal a b c =
    (optimalX a b c, optimalF a b c)

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
