module Main where

import Init
import qualified SDLUI
import SDLUIMInstance ()
import MainState

main = main' 
    (Just (doUI::SDLUI.UIM MainState -> IO (Maybe MainState)))
    (Nothing :: (Maybe (SDLUI.UIM MainState -> IO (Maybe MainState))))
