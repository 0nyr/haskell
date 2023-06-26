module Main where

import Control.Monad (ap)

data CountM a = Counter a Int deriving (Show)

instance Eq a => Eq (CountM a) where
    (Counter x c) == (Counter y c') = x == y && c == c'

-- NOTE: f returns a monad (so returns a count)
instance Monad CountM where
    return x = Counter x 0
    (Counter x c) >>= f = 
        let (Counter y c') = f x -- y is f x wrapped in a monad
        in Counter y (c + 1 + c') -- c + 1 + c' is the total count

instance Functor CountM where
    fmap f x = x >>= pure . f

instance Applicative CountM where
    pure = return
    (<*>) = ap

-- Tests
myAssert :: Bool -> IO ()
myAssert True = putStrLn "🟢 assertion ok"
myAssert False = putStrLn "🔴 assertion failed"

main :: IO ()
main = do
    putStrLn "Week 10 exercices: [see below]"

    print (do { x <- return "abc"; y <- return "def"; return (x++y) } :: CountM String)
    myAssert((do { x <- return "abc"; y <- return "def"; return (x++y) } :: CountM String) == Counter "abcdef" 2)

