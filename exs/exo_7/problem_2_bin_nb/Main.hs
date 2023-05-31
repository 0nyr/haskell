module Main where

-- import BigEndian and LittleEndian and launch the tests
import qualified BigEndian
import qualified LittleEndian

main :: IO ()
main = do
    putStrLn "Week 7 exercices: [see below]"

    -- BigEndian
    putStrLn "\n-- BigEndian tests --"
    BigEndian.main

    -- LittleEndian
    putStrLn "\n-- LittleEndian tests --"
    LittleEndian.main