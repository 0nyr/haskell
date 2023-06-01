module Main where

-- WARN: cannot use Data.Tree since we define our own Tree data
--import Data.Tree

data Tree = E                 -- empty leaf
          | N Tree Char Tree  -- inner node
            deriving Eq

-- a)
-- EX: previous examples of definitions for fold operations:

-- foldl :: (a->b->a) -> a -> [b] -> a
-- foldl f e [] = e
-- foldl f e (x:xs) = foldl f (f e x) xs

-- foldTree :: (a->[b]->b) -> Tree a -> b
-- foldTree f (Node x subtrees) = f x (map (foldTree f) subtrees)

-- foldTree :: (acc -> Char -> acc) -> acc -> (Tree, Tree) -> acc
foldTree :: (String -> Char -> String) -> String -> Tree -> String
foldTree _ accum E = accum
foldTree f accum (N subtreeL c subtreeR) =
    (f accum c) ++ (foldTree f accum subtreeL) ++ (foldTree f accum subtreeR)

-- fold on tree with default value for E case
foldTreeDefaultE :: (String -> Char -> String) -> String -> Tree -> Char -> String
foldTreeDefaultE _ accum E defaultEVal = defaultEVal : accum
foldTreeDefaultE f accum (N subtreeL c subtreeR) dev =
    (f accum c) ++ (foldTreeDefaultE f accum subtreeL dev) ++ (foldTreeDefaultE f accum subtreeR dev)

exampleTree :: Tree
exampleTree = N (N E 'a' E) 'b' E

exampleTree2 :: Tree
exampleTree2 = N (N E 'a' E) 'b' (N (N E 'c' (N E 'd' E)) 'e' E)

preorder :: Tree -> String
preorder tree = foldTreeDefaultE pre "" tree '.'
    where
        pre accum c = c : accum

preorderCorrection :: Tree -> String
preorderCorrection E = "."
preorderCorrection (N subtreeL c subtreeR) = 
    c : (preorderCorrection subtreeL) ++ (preorderCorrection subtreeR)

foldCorrection :: b -> (b -> Char -> b -> b) -> Tree -> b
foldCorrection leaf _ E = leaf
foldCorrection leaf f (N subtreeL c subtreeR) =
    f (foldCorrection leaf f subtreeL) c (foldCorrection leaf f subtreeR)

preorderCorrectionUsingFold :: Tree -> String
preorderCorrectionUsingFold = foldCorrection "." (\l c r -> c : l ++ r)

-- b)
fromPreorder :: String -> Tree
fromPreorder "" = E
fromPreorder (c: substring) = if c /= '.' then
        let Just (treesL, remainingStrAfterL) = explore substring
            Just (treesR, _) = explore remainingStrAfterL
        in
            N treesL c treesR
    else
        E
    where
        explore :: String -> Maybe (Tree, String)
        explore "" = Nothing
        explore (c1: substr1) = if c1 /= '.' then
            let Just (treesL, remainingStrAfterL) = explore substr1
                Just (treesR, remainingStrAfterR) = explore remainingStrAfterL
            in
                Just (N treesL c1 treesR, remainingStrAfterR)
        else
            Just (E, substr1)


myAssert :: Bool -> IO ()
myAssert True = putStrLn "🟢 assertion ok"
myAssert False = putStrLn "🔴 assertion failed"

main :: IO ()
main = do
    putStrLn "Week 5 exercices: [see below]"

    -- a)
    print (preorder exampleTree)
    print (preorder exampleTree2)
    myAssert (preorder exampleTree == "ba...")

    -- b)
    print (preorder (fromPreorder "ba..."))
    print (preorder (fromPreorder "ba..ec.d..."))
    myAssert (fromPreorder "ba..." == exampleTree)
    myAssert (fromPreorder "ba..ec.d..." == exampleTree2)









