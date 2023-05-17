module DC
    ( dc )
where

--
-- Skeleton for divide-and-conquer algorithms
--
dc :: (a -> Bool) -> (a -> b) -> (a -> [a]) -> (a -> [b] -> b) -> a -> b
dc p b d c = dcprog
    where
      dcprog x
          | p x        =  b x
          | otherwise  =  c x $ map dcprog $ d x


