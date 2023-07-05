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
--
eval :: LExp -> Env Domains -> Domains 
eval (V x)     env = value env x 
eval (CInt i)  _   = error "please implement"
eval (CBool b) _   = error "please implement"
eval (f :@: x) env  = error "please implement"
eval (L x v)   env  = error "please implement"
eval (If c t e) env = error "please implement"
eval (Y f) env =
    -- evaluate f in env first, then apply y (see above)
    -- to the result
    error "please implement"
eval (Prim op xs) env = primop op [eval x env | x <- xs]
