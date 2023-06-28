module Reduce where

import Syntax

--
-- Application of a predefined (primitive) operator
--
applyPrim :: Op -> [LExp] -> LExp
applyPrim op xs
 = case (op,xs) of
    (OAnd,[CBool a,CBool b]) -> CBool (a&&b)
    (OOr ,[CBool a,CBool b]) -> CBool (a||b)
    (ONot,[CBool a]        ) -> CBool (not a)
    (OAdd,[CInt a ,CInt b ]) -> CInt  (a+b)
    (OSub,[CInt a ,CInt b ]) -> CInt  (a-b)
    (OMul,[CInt a ,CInt b ]) -> CInt  (a*b)
    (OEq ,[CInt a ,CInt b ]) -> CBool (a==b)
    (OEq ,[CBool a,CBool b]) -> CBool (a==b)
    _ -> error "illegal call to predefined operator"

--
-- Perform a reduction step on the
-- leftmost-outermost redex. Do not look inside
-- a lambda expression. In other words, this
-- function performs one reduction step towards
-- weak head normal form (WHNF). When the given
-- expression is already in WHNF, return Nothing.
--
red_Redex_LMHead :: LExp -> Maybe LExp
red_Redex_LMHead = r
    where
      r (V _)         = Nothing
      r (Prim op xs)  = case red_Redex_LMHead_Sequence xs of
                          Nothing -> Just (applyPrim op xs)
                          Just ys -> Just (Prim op ys)
      r _ = error "please implement"

--
-- Reduces the first expression in a list of
-- expressions which can be reduced using "red_Redex_LMHead".
--
red_Redex_LMHead_Sequence :: [LExp] -> Maybe [LExp]
red_Redex_LMHead_Sequence rs =
    case res of
      Left _   -> Nothing
      Right ys -> Just ys
    where
      f (Left xs) x =
          case red_Redex_LMHead x of
            Nothing -> Left  (xs++[x])
            Just x' -> Right (xs++[x'])
      f (Right xs) x = Right (xs++[x])
      res = foldl f (Left []) rs


--
-- Return the list of reduction steps
-- when reducing the given expression to
-- weak head normal form.
--
reduce_WHNF :: LExp -> [LExp]
reduce_WHNF exp =
    case red_Redex_LMHead exp of
      Nothing   -> [exp]
      Just exp' -> exp :  reduce_WHNF exp'
