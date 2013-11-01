module Main where

import Control.Monad
import System.Exit
import Control.Monad.Trans.State

import Init
import UI
import qualified CursesUI

main = do
    (mlock,opts,mpath) <- Init.init
    initMState <- case mlock of
	    Nothing -> initMetaState
	    Just lock -> return $ newEditState lock mpath
    mFinalState <- 
	(doUI::CursesUI.UIM a -> IO (Maybe a)) $ interactUI `execStateT` initMState
    case mFinalState of
	Nothing -> exitFailure
	Just finalState -> do
	    when (mlock == Nothing) $ writeMetaState finalState
	    exitSuccess
