module Main where

import System.Environment
import System.Exit
import Control.Monad.Writer

import Hex
import Mundanities
import Physics
import Frame
import Lock

main = do
    [lockfn,solutionfn] <- getArgs
    Just lock <- readReadFile lockfn
    Just solution <- readReadFile solutionfn

    if checkSolution (reframe lock) solution then exitSuccess else exitFailure

