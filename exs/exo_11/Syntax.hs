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

--
-- free variables
--
fv :: LExp -> [String]
fv = error "please implement"

--
-- bound variables
--
bv :: LExp -> [String]
bv = error "please implement"


--
-- Replace every free occurrence of v by e.
--
substitute :: String -> LExp -> LExp -> LExp
substitute v e = s
    where
      s :: LExp -> LExp
      s = error "please implement"

--
-- Return a fresh name which does not occur
-- in the given list.
--
occursNot :: [String] -> String
occursNot = error "please implement"
