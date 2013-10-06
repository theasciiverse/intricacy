module Main where

import Control.Monad
import System.Exit
import Control.Monad.Trans.State

import Init
import UI
import qualified SDLUI
import qualified CursesUI

main = do
    (mlock,opts,mpath) <- Init.init
    initMState <- case mlock of
	    Nothing -> initMetaState
	    Just lock -> return $ newEditState lock mpath
    ok <- if ForceCurses `elem` opts
	then (doUI::SDLUI.UIM () -> IO Bool) $ void $ interactUI `runStateT` initMState
	else (doUI::CursesUI.UIM () -> IO Bool) $ void $ interactUI `runStateT` initMState
    if ok then exitSuccess else exitFailure
