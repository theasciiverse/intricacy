module Main where

import System.Environment
import Control.Monad.Writer

import Hex
import Mundanities
import Physics
import Frame
import Lock

type Solution = [PlayerMove]

check :: Lock -> Solution -> Bool
check (frame,st) pms = or $ map (\st' -> checkSolved (frame,st')) $ scanl (((fst.runWriter).).flip physicsTick) st pms

main = do
    [lockfn,solutionfn] <- getArgs
    Just lock <- readReadFile lockfn
    Just solution <- readReadFile solutionfn

    print $ check (reframe lock) solution

