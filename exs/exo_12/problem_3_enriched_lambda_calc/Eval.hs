module Eval where

import Syntax
import Environment

--
-- The possible values for the interpretation of expressions
--
data Domains = I Int 
             | B Bool 
             | F (Domains -> Domains)

instance Show Domains where
 show (I i) = show i
 show (B b) = show b
 show (F x)  = "<function>"

--
-- Fixed-point operator y, can be used in
-- the definition of the semantics of Y-expressions.
-- 
y :: (a->a)->a
y h = h (y h)

--
-- Application of a predefined operator
--
primop :: Op -> [Domains] -> Domains
primop OAnd [B a,B b] = B (a&&b)
primop OOr  [B a,B b] = B (a||b)
primop ONot [B a]     = B (not a)
primop OAdd [I a,I b] = I (a+b)
primop OSub [I a,I b] = I (a-b)
primop OMul [I a,I b] = I (a*b)
primop OEq  [I a,I b] = B (a==b)
primop OEq  [B a,B b] = B (a==b)
primop _ _ = error "illegal call to predefined operator"

--
-- Evaluation function
-- Returns the value of a term in the given environment
-- map a term to the domain
eval :: LExp -> Env Domains -> Domains 
eval (V x)     env = value env x 
eval (CInt i)  _   = I i -- construct a domain integer with the value i 
eval (CBool b) _   = B b -- construct a domain boolean with the value b
eval (f :@: x) env  = -- apply f to the evaluation of x in env
    case eval f env of -- evaluate f in env
        F f' -> f' (eval x env) -- if f is a function, apply it to the evaluation of x in env
        _ -> error "left side of application must be a function"
eval (L x v)   env  = -- addEnv is a king of "let" expression that binds x to v in env
    F (\arg -> eval v (addEnv (x,arg) env)) -- return a function that takes an argument and evaluates v in the environment where x is bound to arg
eval (If c t e) env = 
    case eval c env of  -- evaluate the condition
        B True -> eval t env -- if true, evaluate the then branch
        B False -> eval e env -- if false, evaluate the else branch
        _ -> error "condition in if-expression must be boolean"
eval (Y f) env =
    -- evaluate f in env first, then apply y (see above)
    -- to the result
    case eval f env of
        F g -> y g
        _ -> error "right side of Y-expression must be a function"
eval (Prim op xs) env = primop op [eval x env | x <- xs]
