-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

{-# OPTIONS_GHC -cpp #-}
#ifdef APPLE
{-# LANGUAGE ForeignFunctionInterface #-}
#endif
module Main where

import Init
import qualified SDLUI (UIM)
import SDLUIMInstance ()
import qualified CursesUI (UIM)
import CursesUIMInstance ()
import MainState

#ifdef APPLE
foreign export ccall hs_MAIN :: IO ()

hs_MAIN :: IO ()
hs_MAIN = main
#endif

main = main'
    (Just (doUI::SDLUI.UIM MainState -> IO (Maybe MainState)))
    (Just (doUI::CursesUI.UIM MainState -> IO (Maybe MainState)))
