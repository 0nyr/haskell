module Expr where

import Data.Map ( Map )
import Data.Map as Map

--
-- Syntax of simple expression language
--
data Op = Add | Sub | Mul
          deriving (Show)

data Expr = Const Integer              -- constant value
          | BinOp Op Expr Expr         -- binary operation
          | Var String                 -- variable
          | Lambda String Expr         -- lambda expression: (\var. expr)
          | App Expr Expr              -- application:   expr1 expr2
          | Y Expr                     -- fixed-point operator
          | IfThenElse Expr Expr Expr  --  if expr1 /= 0 then expr2 else expr3
            deriving (Show)

--
-- For a simple implementation (using denotational semantics)
-- of the expression language, we define the possible values
-- of expressions. Each (well-defined) expression represents
-- either an integer or a function.
--
data Val = I Integer
         | F (Val -> Val)

instance Show Val where
    show (I i) = show i
    show (F _) = "<<function>>"

type Env = Map String Val

--
-- To emulate recursion, one has to use a fixed-point operator.
-- For example, "fib = \n -> ... fib ..." is implemented by
-- "y (\fib -> \n -> ... fib)" (i.e., "fib" becomes a leading
-- parameter of the expression and "y" is applied to it).
--
y :: (a -> a) -> a
y f = f (y f)

--
-- Interpret an expression in an environment by translating it
-- into the Val domain.
--
interpret :: Expr -> Env -> Val
interpret (Const i) _   = I i
interpret (Var v)   env =
    case Map.lookup v env of
      Just val -> val
      Nothing  -> error ("Undefined variable " ++ v)
interpret (BinOp op l r) env =
    case (interpret l env, interpret r env) of
      (I lv, I rv) -> I (oper op lv rv)
      _            -> error "type mismatch"
    where
      oper Add = (+)
      oper Sub = (-)
      oper Mul = (*)
interpret (Lambda v e) env =
    -- (\v.e)  is an expression representing a function
    -- from the value for "v" to the value of "e" (with
    -- the value vor "v" added to the environment.
    F (\val -> interpret e (Map.insert v val env))
interpret (App e1 e2) env =
    case interpret e1 env of
      F f -> f (interpret e2 env)
      _   -> error "non-function value applied to an argument"
interpret (Y e) env =
    case interpret e env of
      F f -> y f
      _   -> error "Y applied to non-function"
interpret (IfThenElse c t e) env =
    case interpret c env of
      I 0 -> interpret e env
      _   -> interpret t env

run :: Expr -> Val
run expr = interpret expr Map.empty
