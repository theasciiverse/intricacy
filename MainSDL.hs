module Main where

import Init
import qualified SDLUI
import MainState

main = main' 
    (Just (doUI::SDLUI.UIM MainState -> IO (Maybe MainState)))
    (Nothing :: (Maybe (CursesUI.UIM MainState -> IO (Maybe MainState))))
