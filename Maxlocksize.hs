module Maxlocksize where

-- | Locks bigger than 11 won't fit in an 80x25 terminal
maxlocksize :: Int
maxlocksize = 11
