module Main where

import Control.Monad
import System.Exit

import Init
import UI
import qualified SDLUI
import qualified CursesUI

main = do
    (lock,opts,fname) <- Init.init
    ok <- if ForceCurses `elem` opts
	then runUI $ ((test lock fname)::CursesUI.UIM Bool)
	else runUI $ ((test lock fname)::SDLUI.UIM Bool)
    if ok then exitSuccess else exitFailure
