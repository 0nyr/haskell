module Main where

-- let x = putStr "abc" -- remember that x is not a value, but a computation
-- calling "do {x; x; x}" will output "abcabcabc"

-- To run this program you have the following options:
--  (1) You can load it into GHCi as usual and start it with ":main"
--  (2) Interpret it directly by calling "runhaskell GuessMyNumber.hs" on the
--      command line
--  (3) Compile it using "ghc -o game GuessMyNumber.hs -main-is GuessMyNumber.main"
--      and execute it with "./game" (UNIX syntax)
--
-- compile with: ghc Main.hs -package random

import System.IO
import Text.Read (readMaybe)
import System.Random                    -- needs (-package) random [GHC]

readInput :: IO Int
readInput = do
    putStrLn "Enter a number:"
    str <- getLine
    let maybeInt = readMaybe str :: Maybe Int
    case maybeInt of
        Just i  -> do { putStrLn ("You entered the number: " ++ show i); return i }
        Nothing -> do { putStrLn "That's not a valid number!"; readInput }

userInputCheck :: Int -> IO ()
userInputCheck secret = do
    input <- readInput
    case input == secret of
        True  -> do { putStrLn "You guessed correctly!"; return () }
        False -> do { putStrLn "You guessed incorrectly!";
                        case input < secret of
                            True  -> do { putStrLn "Your guess is too low!";  userInputCheck secret }
                            False -> do { putStrLn "Your guess is too high!"; userInputCheck secret }
                    }
                        

-- correction
game :: Int -> Int -> IO ()
game secret nTry = do
    putStr ("Try #" ++ show nTry ++ ": ")
    input <- readInput
    let guess = read line :: Int
    case guess `compare` secret of
        LT -> putStrLn "Too low!" 
            >> game secret (nTry + 1)
        GT -> putStrLn "Too high!"
            >> game secret (nTry + 1)
        EQ -> putStrLn "You win!"
    

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    
    putStrLn "I think of a number in the range 1 to 100."
    putStrLn "Try to guess which number is it?"
    secret <- randomRIO (1, 100) :: IO Int
    userInputCheck secret


