module InvBin where

data InvBin = Z         -- 0
            | O InvBin  -- n -> 2*n
            | L InvBin  -- n -> 2*n+1
            deriving Show

fromInvBin :: InvBin -> Integer
fromInvBin Z     = 0                     -- fromInvBin.1
fromInvBin (O x) = 2 * fromInvBin x      -- fromInvBin.2
fromInvBin (L x) = 2 * fromInvBin x + 1  -- fromInvBin.3

inc :: InvBin -> InvBin
inc Z     = L Z         -- inc.1
inc (O x) = L x         -- inc.2
inc (L x) = O (inc x)   -- inc.3


--
-- Task 2(d)
--
-- Prove using structural induction that for all finite x :: InvBin
-- the following holds:
--     fromInvBin (inc x) == fromInvBin x + 1
-- (See the file InductionProof.hs in Stud.IP for an explanation how
-- to test your proof in an automated fashion.)
--

proof2d :: InvBin -> [Integer]
proof2d = undefined
