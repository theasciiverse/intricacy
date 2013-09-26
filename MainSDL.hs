module Main where

import Control.Monad

import Init
import UI
import qualified SDLUI
import qualified CursesUI

main = do
    (lock,opts,fname) <- Init.init
    sdlOk <- if ForceCurses `elem` opts
	then return False
	else runUI $ ((test lock fname)::SDLUI.UIM Bool)
    when (not sdlOk) $ void $ runUI $ ((test lock fname)::CursesUI.UIM Bool)
