-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Main where

import Init
import qualified SDLUI
import SDLUIMInstance ()
import MainState

main = main' 
    (Just (doUI::SDLUI.UIM MainState -> IO (Maybe MainState)))
    (Nothing :: (Maybe (SDLUI.UIM MainState -> IO (Maybe MainState))))
