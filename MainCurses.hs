module Main where

import Init
import qualified CursesUI
import UI

main = main' 
    Nothing
    (Just (doUI::CursesUI.UIM a -> IO (Maybe a)))
