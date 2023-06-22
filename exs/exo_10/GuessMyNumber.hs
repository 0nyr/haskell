--
-- To run this program you have the following options:
--  (1) You can load it into GHCi as usual and start it with ":main"
--  (2) Interpret it directly by calling "runhaskell GuessMyNumber.hs" on the
--      command line
--  (3) Compile it using "ghc -o game GuessMyNumber.hs -main-is GuessMyNumber.main"
--      and execute it with "./game" (UNIX syntax)
--
module GuessMyNumber where

import System.IO
import System.Random                    -- needs (-package) random [GHC]


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    -- TODO: please implement...

