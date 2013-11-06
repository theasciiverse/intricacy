module Main where

import Init
import qualified CursesUI (UIM)
import CursesUIMInstance ()
import MainState

main = main' 
    (Nothing :: (Maybe (CursesUI.UIM MainState -> IO (Maybe MainState))))
    (Just (doUI::CursesUI.UIM MainState -> IO (Maybe MainState)))
