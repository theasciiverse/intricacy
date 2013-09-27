module Main where

import Control.Monad
import System.Exit

import Init
import UI
import qualified CursesUI

main = do
    (lock,opts,fname) <- Init.init
    ok <- runUI $ ((test lock fname)::CursesUI.UIM Bool)
    if ok then exitSuccess else exitFailure
