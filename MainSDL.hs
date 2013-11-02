module Main where

import Init
import qualified SDLUI
import UI

main = main' 
    (Just (doUI::SDLUI.UIM a -> IO (Maybe a)))
    Nothing
