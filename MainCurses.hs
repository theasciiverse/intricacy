module Main where

import Control.Monad

import Init
import UI
import qualified CursesUI

main = do
    (lock,opts,fname) <- Init.init
    void $ runUI $ ((test lock fname)::CursesUI.UIM Bool)
