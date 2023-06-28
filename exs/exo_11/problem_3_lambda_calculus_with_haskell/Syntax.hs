module Syntax where

import Data.List


infixl 9 :@:

data Op = OAnd | OOr | ONot | OAdd | OSub | OMul
        | OEq deriving (Eq,Show)

data LExp = V String               -- variable
          | CInt Int               -- Int constant
          | CBool Bool             -- Bool constant
          | LExp :@: LExp          -- application
          | L String LExp          -- abstraction
          | If LExp LExp LExp      -- [If c t e]: if c then t else e
          | Y LExp                 -- fixed point operator
          | Prim Op [LExp]         -- predefined operator
          deriving (Show)

-- the following function is an attempt to make a custom Eq instance for LExp
alphaEquivalent :: LExp -> LExp -> Bool
alphaEquivalent (V x) (V y) = x == y
alphaEquivalent (CInt i) (CInt j) = i == j
alphaEquivalent (CBool b1) (CBool b2) = b1 == b2
alphaEquivalent (f1 :@: x1) (f2 :@: x2) = alphaEquivalent f1 f2 && alphaEquivalent x1 x2
alphaEquivalent (L x1 e1) (L x2 e2) = alphaEquivalent (substitute x1 (V "temp") e1) (substitute x2 (V "temp") e2)
alphaEquivalent (If c1 t1 e1) (If c2 t2 e2) = alphaEquivalent c1 c2 && alphaEquivalent t1 t2 && alphaEquivalent e1 e2
alphaEquivalent (Y e1) (Y e2) = alphaEquivalent e1 e2
alphaEquivalent (Prim op1 exprs1) (Prim op2 exprs2) = op1 == op2 && all id (zipWith alphaEquivalent exprs1 exprs2)
alphaEquivalent _ _ = False


-- NOTE: here, it's a bit of a hack
-- Eq typeclass is typically used for structural equality. 
-- However, since in the context of this exercise, we want to use alpha-equivalence 
-- (a form of semantic equality) instead of structural equality.
-- this is because two lambda expressions are equal if they are alpha-equivalent.
-- ex: λx.x = λy.y

instance Eq LExp where
  (==) = alphaEquivalent

--
-- free variables
--
fv :: LExp -> [String]
fv (V x)          = [x]
fv (CInt _)       = []
fv (CBool _)      = []
fv (f :@: x)      = union (fv f) (fv x)
fv (L x e)        = fv e \\ [x]
fv (If c t e)     = fv c `union` fv t `union` fv e
fv (Y e)          = fv e
fv (Prim _ exprs) = foldr union [] (map fv exprs)

--
-- bound variables
--
bv :: LExp -> [String]
bv (V _)          = []
bv (CInt _)       = []
bv (CBool _)      = []
bv (f :@: x)      = union (bv f) (bv x)
bv (L x e)        = union (bv e) [x]
bv (If c t e)     = bv c `union` bv t `union` bv e
bv (Y e)          = bv e
bv (Prim _ exprs) = foldr union [] (map bv exprs)

--
-- Return a fresh name which does not occur
-- in the given list.
--
occursNot :: [String] -> String
occursNot xs = head $ dropWhile (`elem` xs) candidates
    where
      candidates = ["x" ++ show n | n <- [0..]]

-- This function replaces every free occurrence of a given variable
-- with a provided lambda expression in a target lambda expression.
substitute :: String -> LExp -> LExp -> LExp
substitute targetVar replacementExpr = substituteIn
    where
      substituteIn :: LExp -> LExp
      substituteIn (V var)
        -- If the current variable is the target variable, replace it with the replacement expression.
        | var == targetVar = replacementExpr
        -- If the current variable is not the target variable, leave it as is.
        | otherwise = V var
      -- Apply substitution in both sides of an application.
      substituteIn (expr1 :@: expr2) = (substituteIn expr1) :@: (substituteIn expr2)
      substituteIn (L var expr)
        -- If the current variable is the target variable, it's bound here 
        -- and shouldn't be replaced within this expression.
        | var == targetVar = L var expr
        -- If the current variable is not the target variable, but occurs in 
        -- the replacement expression, we need to avoid capturing it 
        -- by renaming it in this expression.
        | var `elem` (fv replacementExpr) = 
            let 
              newVar = occursNot (fv expr ++ fv replacementExpr ++ bv expr)
            in 
              L newVar (substituteIn (substitute var (V newVar) expr))
        -- If the current variable is not the target variable and does not occur in the replacement expression,
        -- just apply substitution in the body of the abstraction.
        | otherwise = L var (substituteIn expr)
      substituteIn (If condExpr thenExpr elseExpr) =
        If (substituteIn condExpr) (substituteIn thenExpr) (substituteIn elseExpr)
      substituteIn (Y expr) = Y (substituteIn expr)
      substituteIn (Prim op exprs) = Prim op (map substituteIn exprs)
      -- For constants, return them as is.
      substituteIn const@(CInt _) = const
      substituteIn const@(CBool _) = const




