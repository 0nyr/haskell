module Environment(Env,value,addEnv,emptyEnv) where

--
-- Mappings for variables, i.e., (name,value)-pairs
--
data Env a = E { content::[(String,a)] }

value :: Env a -> String -> a
value env key = case lookup key (content env) of
                 Nothing -> error "name not found in environment"
                 Just x  -> x

--
-- Add a variable mapping to the environment
--
addEnv :: (String,a) -> Env a -> Env a
addEnv  x (E xs) = E (x:xs)

--
-- The empty environment
--
emptyEnv :: Env a
emptyEnv = E []

